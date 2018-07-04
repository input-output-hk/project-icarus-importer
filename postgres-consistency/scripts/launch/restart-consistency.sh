#!/usr/bin/env bash

# Requires:
#   - Having the node key-value db up-to-date
# Usage:
#   ./restart-consistency.sh CHAIN RESTART_NUM MIN_BETWEEN_RESTART
#                            IMPORTER_KV_DB_LOCATION NODE_KV_DB_LOCATION

scriptDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
repoDir="${scriptDir}/../../.."
logsFile="${repoDir}/postgres-consistency/restartConsistency.log"
topologyFile="/tmp/restart-topology.yaml"

. ${scriptDir}/utils.sh

# Parameters
chain="$1"
restartNumber="$2"
minutesBetweenRestart="$3"
kvDBLocationImporter="$4"
kvDBLocationNode="$5"

get_importer_height () {
  height=$(curl -s -H 'Content-Type: application/json' -X GET http://localhost:8200/api/stats/blocksCount | jq .Right)
  return ${height}
}

CONFIG_KEY=
KEY_FILE=
TOPOLOGY_HOST=
setup_chain_config ${scriptDir} ${chain}

logWithTimestamp "Doing setup"
${repoDir}/scripts/build/cardano-sl.sh blockchain-importer > /dev/null
printf "wallet:\n relays: [[{ host: ${TOPOLOGY_HOST} }]]\n valency: 1\n fallbacks: 7" > ${topologyFile}

for i in $(eval echo {1..$restartNumber})
  do
    logWithTimestamp "${i}: Starting node for ${i}th time"
    stack exec -- cardano-blockchain-importer \
      --topology "${topologyFile}" \
      --log-config "${repoDir}/blockchain-importer/log-config.yaml" \
      --logs-prefix "${repoDir}/restart-logs" \
      --db-path "${kvDBLocationImporter}" \
      --keyfile "${repoDir}/${KEY_FILE}" \
      --configuration-file "${repoDir}/lib/configuration.yaml" \
      --configuration-key ${CONFIG_KEY} \
      --postgres-name ${DB} --postgres-password ${DB_PASSWORD} \
      --postgres-host ${DB_HOST} --postgres-port ${DB_PORT} > /dev/null &

    get_importer_height
    logWithTimestamp "${i}: Waiting for blockchain-importer to import blocks, starting with $? blocks"
    sleep ${minutesBetweenRestart}m
    
    get_importer_height
    logWithTimestamp "${i}: Stopping node with height $?"
    kill -9 $!
 
    logWithTimestamp "${i}: Starting restart consistency test"
    ${scriptDir}/internal-consistency.sh ${chain} ${kvDBLocationImporter} ${kvDBLocationNode} > ${logsFile}
    
    consistencyResultSucceeded=$(grep "All internal consistency checks succeded" ${logsFile})
    if [ "${consistencyResultSucceeded}" != "" ]; then
      logWithTimestamp "${i}: Restart consistency check succeeded"
      rm ${logsFile}
    else
      logWithTimestamp "${i}: Restart consistency check failed. Check logs on file ${logsFile}"
      exit
    fi
  done
