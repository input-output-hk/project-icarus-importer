#!/usr/bin/env bash

# Requires:
#   - Having the node key-value db up-to-date
#   - Having the postgres db up-to-date
#   - Configuring the environment values for the postgres db (i.e.: source setup-localDB.sh)
# Usage:
#   ./external-consistency.sh TOP_EPOCH NUMBER_BLOCKS KV_DB_LOCATION

# TODO
# - Add setting up using local or staging db
# - Add setting up using mainnet or staging (requires configuring the EpochAndSlot generator)

scriptDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
repoDir="${scriptDir}/../../.."
blkFile="${repoDir}/postgres-consistency/blkHashes.txt"
logsFile="${repoDir}/postgres-consistency/externalConsistency.log"

# Parameters
topEpoch="$1"
numberBlocks="$2"
kvDBLocation="$3"

# FIXME: Use also in external consistency? Import from utils file
logWithTimestamp () {
  echo "[$(date --rfc-3339='ns')] $1"
}


logWithTimestamp "Getting random blocks to check"
node ${scriptDir}/../EpochSlotToBlkHash.js ${topEpoch} ${numberBlocks} > ${blkFile}

logWithTimestamp "Doing setup"
${repoDir}/scripts/build/cardano-sl.sh postgres-consistency > /dev/null
printf "wallet:\n relays: [[{ host: relays.awstest.iohkdev.io }]]\n valency: 1\n fallbacks: 7" > /tmp/topology-staging.yaml

logWithTimestamp "Running external consistency test"
stack exec -- cardano-postgres-consistency ext-const --blocks-file ${blkFile} \
           --topology "/tmp/topology-staging.yaml" \
           --log-config "${repoDir}/blockchain-importer/log-config.yaml" \
           --logs-prefix "${repoDir}/logs-staging" \
           --db-path ${kvDBLocation} \
           --keyfile "${repoDir}/secret-staging.key" \
           --configuration-file "${repoDir}/lib/configuration.yaml" \
           --configuration-key mainnet_dryrun_full \
           --postgres-name ${DB} --postgres-password ${DB_PASSWORD} \
           --postgres-host ${DB_HOST} --postgres-port ${DB_PORT} > ${logsFile}

logWithTimestamp "Checking result obtained"
consistencyResultSucceeded=$(grep "Consistency check succeeded" ${logsFile})
consistencyResultFailed=$(grep "Consistency check failed" ${logsFile})
if [ "${consistencyResultSucceeded}" != "" ]; then
  logWithTimestamp "Consistency check succeeded"
  rm ${logsFile}
elif [ "${consistencyResultFailed}" != "" ]; then
  logWithTimestamp "Consistency check failed. Check logs on file ${logsFile}."
else
  logWithTimestamp "Unknown error happened. Check logs on file ${logsFile}."
fi

# Clean up
rm ${blkFile}
