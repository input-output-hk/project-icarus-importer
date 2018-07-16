#!/usr/bin/env bash

logWithTimestamp () {
  echo "[$(date --rfc-3339='ns')] $1"
}

check_succeded_on_logs () {
  logWithTimestamp "Checking result obtained from $1"
  consistencyResultSucceeded=$(grep "Consistency check succeeded" $2)
  consistencyResultFailed=$(grep "Consistency check failed" $2)
  if [ "${consistencyResultSucceeded}" != "" ]; then
    logWithTimestamp "Consistency check $1 succeeded"
    rm ${logsFile}
    return 1
  elif [ "${consistencyResultFailed}" != "" ]; then
    logWithTimestamp "Consistency check failed. Check logs on file $2."
    return 0
  else
    logWithTimestamp "Unknown error happened. Check logs on file $2."
    return 0
  fi
}

# Chain options: 'mainnet', 'staging'
setup_chain_config () {
  chain="$1"

  # Setup node configuration
  if [ ${chain} = "staging" ]; then
    CONFIG_KEY="mainnet_dryrun_full"
    KEY_FILE="secret-staging.key"
    TOPOLOGY_HOST="relays.awstest.iohkdev.io"
  elif [ ${chain} = "mainnet" ]; then
    CONFIG_KEY="mainnet_full"
    KEY_FILE="secret-mainnet.key"
    TOPOLOGY_HOST="relays.cardano-mainnet.iohk.io"
  else
    logWithTimestamp "Invalid chain ${chain} selected, choose between 'mainnet' and 'staging'"
    exit
  fi
  
}
