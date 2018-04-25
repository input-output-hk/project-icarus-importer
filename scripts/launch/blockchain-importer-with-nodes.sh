#!/usr/bin/env bash

system_start=$((`date +%s` + 15))

system_start=$system_start ./scripts/launch/demo.sh & PIDNODE=$!
system_start=$system_start ./scripts/launch/blockchain-importer.sh & PIDEX=$!

wait $PIDEX
wait $PIDNODE
