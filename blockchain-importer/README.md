# `cardano-sl-blockchain-importer`

Package based on the `cardano-sl-explorer` (in how it's hooked to the Cardano node), which keeps a postgres db up-to-date with it's rocks db, saving in it:
- The current UTxO.
- The best block number.
- The tx history (including successful, failed and pending txs).

## Installation

### Requirements

Installation of `nix` is needed.

```bash
curl https://nixos.org/nix/install | sh
source ~/.nix-profile/etc/profile.d/nix.sh
```

Make sure that `nix` is set to `true` within `~/.stack/config.yaml`.

```
nix:
  enable: true
```

## Generate documentation

Generated documentation for BlockchainImporter Web API can be obtained (as `blockchain-importer-web-api-swagger.json`) by running:
```bash
stack exec cardano-importer-swagger
```

## Run mock server

```bash
stack exec cardano-blockchain-importer-mock
```

## Run it

**Pre-requesites**: `sudo apt-get install liblzma-dev libpq-dev`

Run it from project root.

### Dev version

- run `./scripts/build/cardano-sl.sh blockchain-importer`
- Using `stack exec` to use the importer, such as in:
```bash
stack exec -- cardano-blockchain-importer --topology "/tmp/topology-staging.yaml" --log-config "blockchain-importer/log-config.yaml" --logs-prefix "logs" --db-path "db-importer" --keyfile "secret-staging.key" --configuration-file "lib/configuration.yaml" --configuration-key mainnet_dryrun_full --postgres-name "stagingpgdb" --postgres-password "mysecretpassword" --postgres-host "localhost"
```

### Prod version (connects BlockchainImporter to `staging` or `mainnet`)

- Run `/scripts/clean/db.sh` to do a clean synchronization, so that BlockchainImporter will sync and download blockchain from start. Create a 
- Connect to cluster as described in  `docs/how-to/connect-to-cluster.md` **FIXME: Add usage of nix for building, currently doesn't work**
