{ environment ? "mainnet"
, localLib ? import ./../../../lib.nix
, stateDir ? localLib.maybeEnv "CARDANO_STATE_DIR" "state-${executable}-${environment}"
, config ? {}
, executable ? "wallet"
, topologyFile ? null
, system ? builtins.currentSystem
, pkgs ? import localLib.fetchNixPkgs { inherit system config; }
, gitrev ? localLib.commitIdFromGitRepo ./../../../.git
, walletListen ? "127.0.0.1:8090"
, ekgListen ? "127.0.0.1:8000"
, ghcRuntimeArgs ? "-N2 -qg -A1m -I0 -T"
, additionalNodeArgs ? ""
, confKey ? null
, relays ? null
, extraParams ? ""
}:

with localLib;

# TODO: DEVOPS-159: relays DNS should be more predictable
# TODO: DEVOPS-499: developer clusters based on runtime JSON
# TODO: DEVOPS-462: exchanges should use a different topology

let
  environments = {
    mainnet = {
      relays = "relays.cardano-mainnet.iohk.io";
      confKey = "mainnet_full";
      genesisJson = "mainnet-genesis.json";
    };
    mainnet-staging = {
      relays = "relays.awstest.iohkdev.io";
      confKey = "mainnet_dryrun_full";
      genesisJson = "mainnet-genesis.json";
    };
    testnet = {
      relays = "relays.cardano-testnet.iohkdev.io";
      confKey = "testnet_full";
      genesisJson = "testnet-genesis.json";
    };
    demo = {
      confKey = "dev";
    };
    override = {
      inherit relays confKey;
    };
  };

  ourEnvironment = environments.${environment};

  executables = {
    wallet = "${iohkPkgs.cardano-sl-wallet-new}/bin/cardano-node";
    explorer = "${iohkPkgs.cardano-sl-explorer-static}/bin/cardano-explorer";
    blockchain-importer = "${iohkPkgs.cardano-sl-blockchain-importer-static}/bin/cardano-blockchain-importer";
  };
  ifWallet = localLib.optionalString (executable == "wallet");
  iohkPkgs = import ./../../../default.nix { inherit config system pkgs gitrev; };
  src = ./../../../.;
  topologyFileDefault = pkgs.writeText "topology-${environment}" ''
    wallet:
      relays: [[{ host: ${ourEnvironment.relays} }]]
      valency: 1
      fallbacks: 7
  '';
  configFiles = pkgs.runCommand "cardano-config" {} ''
    mkdir -pv $out
    cd $out
    cp -vi ${iohkPkgs.cardano-sl.src + "/configuration.yaml"} configuration.yaml
    cp -vi ${iohkPkgs.cardano-sl.src + "/mainnet-genesis-dryrun-with-stakeholders.json"} mainnet-genesis-dryrun-with-stakeholders.json
    cp -vi ${iohkPkgs.cardano-sl.src}/${ourEnvironment.genesisJson} ${ourEnvironment.genesisJson}
    cp -vi ${iohkPkgs.cardano-sl.src + "/../log-configs/connect-to-cluster.yaml"} log-config-connect-to-cluster.yaml
    cp -vi ${if topologyFile != null then topologyFile else topologyFileDefault } topology.yaml
  '';
in pkgs.writeScript "${executable}-connect-to-${environment}" ''
  #!${pkgs.stdenv.shell}

  mkdir -p ${stateDir}

  if [[ ! -w ${stateDir} || ! -x ${stateDir} ]]; then
    echo "The state dir (${stateDir}) must be owned by user id $(id -u), exiting" 2>&1
    exit 1
  fi

  if [[ "$1" == "--delete-state" ]]; then
    echo "Deleting ${stateDir} ... "
    rm -Rf ${stateDir}
  fi
  if [[ "$2" == "--runtime-args" ]]; then
    RUNTIME_ARGS=$3
  else
    RUNTIME_ARGS=""
  fi

  echo RUNTIME_ARGS are: $RUNTIME_ARGS

  echo "Keeping state in ${stateDir}"
  mkdir -p ${stateDir}/logs

  echo "Launching a node connected to '${environment}' ..."
  ${ifWallet ''
  export LC_ALL=en_GB.UTF-8
  export LANG=en_GB.UTF-8
  if [ ! -d ${stateDir}/tls ]; then
    mkdir ${stateDir}/tls/
    ${pkgs.openssl}/bin/openssl req -x509 -newkey rsa:2048 -keyout ${stateDir}/tls/server.key -out ${stateDir}/tls/server.cert -days 3650 -nodes -subj "/CN=localhost"
  fi
  ''}


  ${executables.${executable}}                                     \
    --configuration-file ${configFiles}/configuration.yaml         \
    --configuration-key ${ourEnvironment.confKey}     \
    ${ ifWallet "--tlscert ${stateDir}/tls/server.cert"}           \
    ${ ifWallet "--tlskey ${stateDir}/tls/server.key"}             \
    ${ ifWallet "--tlsca ${stateDir}/tls/server.cert"}             \
    --log-config ${configFiles}/log-config-connect-to-cluster.yaml \
    --topology "${configFiles}/topology.yaml"                      \
    --logs-prefix "${stateDir}/logs"                               \
    --db-path "${stateDir}/db"   ${extraParams}                    \
    ${ ifWallet "--wallet-db-path '${stateDir}/wallet-db'"}        \
    --keyfile ${stateDir}/secret.key                               \
    ${ ifWallet "--wallet-address ${walletListen}" }               \
    --ekg-server ${ekgListen} --metrics                            \
    +RTS ${ghcRuntimeArgs} -RTS                                    \
    ${additionalNodeArgs}                                          \
    $RUNTIME_ARGS
''
