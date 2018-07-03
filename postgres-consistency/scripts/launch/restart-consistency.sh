#!/usr/bin/env bash

# Requires:
#   - Having the node key-value db up-to-date
#   - Configuring the environment values for the postgres db (i.e.: source setup-localDB.sh)
# Usage:
#   ./restart-consistency.sh RESTART_NUM MIN_BETWEEN_RESTART IMPORTER_KV_DB_LOCATION NODE_KV_DB_LOCATION

# TODO
# - Add setting up using local or staging db
# - Add setting up using mainnet or staging

scriptDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
repoDir="${scriptDir}/../../.."
logsFile="${repoDir}/postgres-consistency/restartConsistency.log"

. ${scriptDir}/utils.sh

# Parameters
restartNumber="$1"
minutesBetweenRestart="$2"
kvDBLocationImporter="$3"
kvDBLocationNode="$4"

logWithTimestamp "Doing setup"
${repoDir}/scripts/build/cardano-sl.sh blockchain-importer > /dev/null
printf "wallet:\n relays: [[{ host: relays.awstest.iohkdev.io }]]\n valency: 1\n fallbacks: 7" > /tmp/topology-staging.yaml

for i in $(eval echo {1..$restartNumber})
  do
    logWithTimestamp "Starting node for ${i}th time"
    stack exec -- cardano-blockchain-importer \
      --topology "/tmp/topology-staging.yaml" \
      --log-config "${repoDir}/blockchain-importer/log-config.yaml" \
      --logs-prefix "${repoDir}/logs-staging" \
      --db-path "${kvDBLocationImporter}" \
      --keyfile "${repoDir}/secret-staging.key" \
      --configuration-file "${repoDir}/lib/configuration.yaml" \
      --configuration-key mainnet_dryrun_full \
      --postgres-name ${DB} --postgres-password ${DB_PASSWORD} \
      --postgres-host ${DB_HOST} --postgres-port ${DB_PORT} > /dev/null &

    logWithTimestamp "Waiting for blockchain-importer to import blocks"
    sleep ${minutesBetweenRestart}m
    # FIXME: Add curl -H 'Content-Type: application/json' -X GET http://localhost:8200/api/stats/blocksCount to get number of blocks

    logWithTimestamp "Stopping node"
    kill -9 $!
 
    logWithTimestamp "Starting restart consistency test"
    ${scriptDir}/internal-consistency.sh ${kvDBLocationImporter} ${kvDBLocationNode} > ${logsFile}
    
    consistencyResultSucceeded=$(grep "All internal consistency checks succeded" ${logsFile})
    if [ "${consistencyResultSucceeded}" != "" ]; then
      logWithTimestamp "Restart consistency check succeeded"
      rm ${logsFile}
    else
      logWithTimestamp "Restart consistency check failed. Check logs on file ${logsFile}"
      exit
    fi
  done
