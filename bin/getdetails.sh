#!/bin/bash

COMMIT_ID=$1
REPOSITORY_LOCATION=$2

git --git-dir="$REPOSITORY_LOCATION" show $COMMIT_ID | grep -v "^index" | md5sum
