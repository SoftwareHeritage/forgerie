#!/bin/bash

# Gets the details of a commit for a given repository, in md5sum form.
# This is useful to see if two commits, from two different repositories, represent
# the same diff.  The primary use case for this is checking when a phabricator
# commit in its staging repository matches a commit in the main repository, in order
# to build a differential chain.  Because the staging repository has a lot more commits,
# the index is usually more characters, meaning we need to grep out the "^index" lines
# in order to build hashes that do match.

COMMIT_ID=$1
REPOSITORY_LOCATION=$2

git --git-dir="$REPOSITORY_LOCATION" show $COMMIT_ID | grep -v "^index" | md5sum
