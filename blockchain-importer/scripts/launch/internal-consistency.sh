#!/usr/bin/env bash

# Requires:
#   - Having the node key-value db up-to-date
#   - Configuring the environment values for the postgres db (i.e.: source setup-localDB.sh)
# Usage:
#   ./internal-consistency.sh CHAIN IMPORTER_KV_DB_LOCATION NODE_KV_DB_LOCATION

scriptDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
repoDir="${scriptDir}/../../.."
logsFile="${repoDir}/blockchain-importer/internalConsistency.log"
topologyFile="/tmp/internal-topology.yaml"

. ${scriptDir}/utils.sh

# Parameters
chain="$1"
kvDBLocationImporter="$2"
kvDBLocationNode="$3"

CONFIG_KEY=
KEY_FILE=
TOPOLOGY_HOST=
setup_chain_config ${chain}

logWithTimestamp "Doing setup"
${repoDir}/scripts/build/cardano-sl.sh blockchain-importer > /dev/null
printf "wallet:\n relays: [[{ host: ${TOPOLOGY_HOST} }]]\n valency: 1\n fallbacks: 7" > ${topologyFile}

logWithTimestamp "Running internal consistency test"
stack exec -- cardano-importer-db-consistency int-const \
           --topology "${topologyFile}" \
           --log-config "${repoDir}/blockchain-importer/log-config.yaml" \
           --logs-prefix "${repoDir}/internal-logs" \
           --db-path ${kvDBLocationImporter} \
           --keyfile "${repoDir}/${KEY_FILE}" \
           --configuration-file "${repoDir}/lib/configuration.yaml" \
           --configuration-key ${CONFIG_KEY} \
           --postgres-name ${DB} --postgres-password ${DB_PASSWORD} \
           --postgres-host ${DB_HOST} --postgres-port ${DB_PORT} > ${logsFile}

check_succeded_on_logs "Internal consistency" ${logsFile}

# If Internal consistency check failed, exit
if [ $? = 0 ]; then
  exit
fi

logWithTimestamp "Getting hash of the tip block"
stack exec -- cardano-importer-db-consistency get-tip-hash \
           --topology "${topologyFile}" \
           --log-config "${repoDir}/blockchain-importer/log-config.yaml" \
           --logs-prefix "${repoDir}/internal-logs" \
           --db-path ${kvDBLocationImporter} \
           --keyfile "${repoDir}/${KEY_FILE}" \
           --configuration-file "${repoDir}/lib/configuration.yaml" \
           --configuration-key ${CONFIG_KEY} \
           --postgres-name ${DB} --postgres-password ${DB_PASSWORD} \
           --postgres-host ${DB_HOST} --postgres-port ${DB_PORT} > ${logsFile}

tipHash=$(grep -oP "Tip hash: [A-Za-z0-9]{64}$" ${logsFile})
tipHash=${tipHash:10}
rm ${logsFile}

logWithTimestamp "Running external tx range consistency test"
stack exec -- cardano-importer-db-consistency ext-range-const --tip-hash "${tipHash}" \
           --topology "${topologyFile}" \
           --log-config "${repoDir}/blockchain-importer/log-config.yaml" \
           --logs-prefix "${repoDir}/internal-logs" \
           --db-path ${kvDBLocationNode} \
           --keyfile "${repoDir}/${KEY_FILE}" \
           --configuration-file "${repoDir}/lib/configuration.yaml" \
           --configuration-key ${CONFIG_KEY} \
           --postgres-name ${DB} --postgres-password ${DB_PASSWORD} \
           --postgres-host ${DB_HOST} --postgres-port ${DB_PORT} > ${logsFile}

check_succeded_on_logs "External range consistency" ${logsFile}
if [ $? = 1 ]; then
  logWithTimestamp "All internal consistency checks succeded"
fi
