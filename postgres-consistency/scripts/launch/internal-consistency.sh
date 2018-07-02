#!/usr/bin/env bash

# Requires:
#   - Having the node key-value db up-to-date
#   - Configuring the environment values for the postgres db (i.e.: source setup-localDB.sh)
# Usage:
#   ./internal-consistency.sh IMPORTER_KV_DB_LOCATION NODE_KV_DB_LOCATION

set -x

scriptDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Parameters
kvDBLocationImporter="$1"
kvDBLocationNode="$2"


# Start topology
printf "wallet:\n relays: [[{ host: relays.awstest.iohkdev.io }]]\n valency: 1\n fallbacks: 7" > /tmp/topology-staging.yaml

# Run internal consistency test
# FIXME: Paths are dependant on from where this is called
stack exec -- cardano-postgres-consistency int-const \
           --topology "/tmp/topology-staging.yaml" \
           --log-config "blockchain-importer/log-config.yaml" \
           --logs-prefix "/home/ntallar/Documents/projects/icarus/logs-staging" \
           --db-path ${kvDBLocationImporter} \
           --keyfile "secret-staging.key" --configuration-file "lib/configuration.yaml" \
           --configuration-key mainnet_dryrun_full \
           --postgres-name ${DB} --postgres-password ${DB_PASSWORD} \
           --postgres-host ${DB_HOST} --postgres-port ${DB_PORT}

# Get hash of the tip block
# FIXME: Obtain result
# FIXME: Paths are dependant on from where this is called
stack exec -- cardano-postgres-consistency get-tip-hash \
           --topology "/tmp/topology-staging.yaml" \
           --log-config "blockchain-importer/log-config.yaml" \
           --logs-prefix "/home/ntallar/Documents/projects/icarus/logs-staging" \
           --db-path ${kvDBLocationImporter} \
           --keyfile "secret-staging.key" --configuration-file "lib/configuration.yaml" \
           --configuration-key mainnet_dryrun_full \
           --postgres-name ${DB} --postgres-password ${DB_PASSWORD} \
           --postgres-host ${DB_HOST} --postgres-port ${DB_PORT}

# Run external tx range consistency test
# FIXME: Paths are dependant on from where this is called
stack exec -- cardano-postgres-consistency ext-range-const --tip-hash "98c588922865ca25a174737cc74f4b326e5c8844a419ca2f08df75263fe5aad5" \
           --topology "/tmp/topology-staging.yaml" \
           --log-config "blockchain-importer/log-config.yaml" \
           --logs-prefix "/home/ntallar/Documents/projects/icarus/logs-staging" \
           --db-path ${kvDBLocationNode} \
           --keyfile "secret-staging.key" --configuration-file "lib/configuration.yaml" \
           --configuration-key mainnet_dryrun_full \
           --postgres-name ${DB} --postgres-password ${DB_PASSWORD} \
           --postgres-host ${DB_HOST} --postgres-port ${DB_PORT}