#!/bin/bash

# This script pulls the database from software heritages production server
# which requires ssh access, as well as mysql access.  This is most likely
# not going to be useful to anyone in the future, especially after the project
# is over, but is here for posterities sake.

# You should also have installed phabricator locally for this to be any use.

SERVER="forge.softwareheritage.org"

ssh $SERVER "/srv/phabricator/phabricator/bin/storage dump --compress --no-indexes --output phabbackup.sql.gz"
scp $SERVER:phabbackup.sql.gz .
gunzip phabbackup.sql.gz
mysql < phabbackup.sql

# This one takes a while
rm -rf /srv/phabricator
ssh $SERVER "tar -zc -C /srv phabricator/repos/" | tar -zx -C /srv
