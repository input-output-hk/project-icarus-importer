#!/usr/bin/env bash

# Requires:
#   - Having the node key-value db up-to-date
#   - Having the postgres db up-to-date
#   - Configuring the environment values for the postgres db (i.e.: source setup-localDB.sh)
# Usage:
#   ./external-consistency.sh CHAIN TOP_EPOCH NUMBER_BLOCKS KV_DB_LOCATION

scriptDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
repoDir="${scriptDir}/../../.."
blkFile="${repoDir}/postgres-consistency/blkHashes.txt"
logsFile="${repoDir}/postgres-consistency/externalConsistency.log"
topologyFile="/tmp/external-topology.yaml"

. ${scriptDir}/utils.sh

# Parameters
chain="$1"
topEpoch="$2"
numberBlocks="$3"
kvDBLocation="$4"

CONFIG_KEY=
KEY_FILE=
TOPOLOGY_HOST=
setup_chain_config ${scriptDir} ${chain}

# FIXME: Do npm install node-fetch?
logWithTimestamp "Doing setup"
${repoDir}/scripts/build/cardano-sl.sh postgres-consistency > /dev/null
printf "wallet:\n relays: [[{ host: ${TOPOLOGY_HOST} }]]\n valency: 1\n fallbacks: 7" > ${topologyFile}

logWithTimestamp "Getting random blocks to check"
node ${scriptDir}/../EpochSlotToBlkHash.js ${topEpoch} ${numberBlocks} > ${blkFile}

logWithTimestamp "Running external consistency test"
stack exec -- cardano-postgres-consistency ext-const --blocks-file ${blkFile} \
           --topology "${topologyFile}" \
           --log-config "${repoDir}/blockchain-importer/log-config.yaml" \
           --logs-prefix "${repoDir}/external-logs" \
           --db-path ${kvDBLocation} \
           --keyfile "${repoDir}/${KEY_FILE}" \
           --configuration-file "${repoDir}/lib/configuration.yaml" \
           --configuration-key ${CONFIG_KEY} \
           --postgres-name ${DB} --postgres-password ${DB_PASSWORD} \
           --postgres-host ${DB_HOST} --postgres-port ${DB_PORT} > ${logsFile}

check_succeded_on_logs "External consistency" ${logsFile}

# Clean up
rm ${blkFile}
