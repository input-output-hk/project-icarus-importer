#!/usr/bin/env bash

# Requires:
#   - Having the node key-value db up-to-date
#   - Having the postgres db up-to-date
#   - Configuring the environment values for the postgres db (i.e.: source setup-localDB.sh)
# Usage:
#   ./external-consistency.sh TOP_EPOCH NUMBER_BLOCKS KV_DB_LOCATION

set -x

scriptDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
blkFile="${scriptDir}/../../blkHashes.txt"

# Parameters
topEpoch="$1"
numberBlocks="$2"
kvDBLocation="$3"


# Get random blocks
node ${scriptDir}/../EpochSlotToBlkHash.js ${topEpoch} ${numberBlocks} > ${blkFile}

# Start topology
printf "wallet:\n relays: [[{ host: relays.awstest.iohkdev.io }]]\n valency: 1\n fallbacks: 7" > /tmp/topology-staging.yaml

# Run consistency test
# FIXME: Paths are dependant on from where this is called
stack exec -- cardano-postgres-consistency ext-const --blocks-file ${blkFile} \
           --topology "/tmp/topology-staging.yaml" \
           --log-config "blockchain-importer/log-config.yaml" \
           --logs-prefix "/home/ntallar/Documents/projects/icarus/logs-staging" \
           --db-path ${kvDBLocation} \
           --keyfile "secret-staging.key" --configuration-file "lib/configuration.yaml" \
           --configuration-key mainnet_dryrun_full \
           --postgres-name ${DB} --postgres-password ${DB_PASSWORD} \
           --postgres-host ${DB_HOST} --postgres-port ${DB_PORT}

# Clean up
rm ${blkFile}
