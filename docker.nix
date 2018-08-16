{ environment ? "mainnet"
, connect
, gitrev
, pkgs
, connectArgs ? {}
}:

with pkgs.lib;

let
  connectToCluster = connect ({
    inherit gitrev environment;
    stateDir = "/wallet/${environment}";
    walletListen = "0.0.0.0:8090";
    ekgListen = "0.0.0.0:8000";
  } // connectArgs);
  startScript = pkgs.writeScriptBin "cardano-start" ''
    #!/bin/sh
    set -e
    set -o pipefail
    if [ ! -d /wallet ]; then
      echo '/wallet volume not mounted, you need to create one with `docker volume create` and pass the correct -v flag to `docker run`'
    exit 1
    fi
    cd /wallet
    exec ${connectToCluster} "$@"
  '';

  user = "cardano-sl";
  uidGidStr = toString 999;

  wait-for-it =
    let file = pkgs.fetchurl {
      url = "https://raw.githubusercontent.com/vishnubob/wait-for-it/db049716e42767d39961e95dd9696103dca813f1/wait-for-it.sh";
      sha256 = "0p43i1bm1yh90pjcyc19i5rmf3bm23bzyi4ppcrsjdwwkmfdwx8g";
    };
    in pkgs.runCommand "wait-for-it" {} ''
      mkdir -p $out/bin
      install "${file}" $out/bin/wait-for-it.sh
      substituteInPlace $out/bin/wait-for-it.sh \
        --replace "which" "type -p"
      patchShebangs $out/bin/wait-for-it.sh
    '';

in pkgs.dockerTools.buildImage {
  name = "cardano-container-${environment}";
  contents = with pkgs; [
    iana-etc
    startScript
    openssl
    bashInteractive
    coreutils
    utillinux
    iproute
    iputils
    curl
    socat
    wait-for-it
  ];
  config = {
    Cmd = [ "cardano-start" ];
    ExposedPorts = {
      "3000/tcp" = {}; # protocol
      "8090/tcp" = {}; # wallet
      "8100/tcp" = {}; # explorer api
      "8200/tcp" = {}; # blockchain-importer
      "8000/tcp" = {}; # ekg
    };
    User = user;
  };
  runAsRoot = ''
    # the user our program is run as
    echo "${user}::${uidGidStr}:${uidGidStr}::::" > /etc/passwd
    # also gets a group
    echo "${user}:x:${uidGidStr}:${user}" > /etc/group
    # create a first wallet folder, in case no volume is mounted
    mkdir /wallet
    chown ${uidGidStr}:${uidGidStr} /wallet
  '';
}
