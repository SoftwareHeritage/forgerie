#!/usr/bin/env bash

logpath="/tmp/forgerie/run-$(date +%Y%m%d-%H%M).log";
echo "## Running migration logs in $logpath ##";

time /opt/forgerie/bin/run | tee $logpath
