(in-package #:forgerie-phabricator)

(defvar *database-username* nil
 "Username to access the database.  If NIL, will use the current user")
(defvar *database-password* nil
 "Password to access the mysql database.  If NIL, will not enter password")

(defvar *revisions-to-skip* nil
 "List of revisions to skip.  For instance, if they cause errors due to weird git
 history.  They need to be handled manually.  The list is of database ids.")

(defvar *pastes-to-skip* nil
 "Pastes that can't be migrated, and will need to be handled manually.  This is a list of
 database ids.  They need to be handled manually.")

; This is the http location of the phabricator server
(defvar *phabricator-location* nil
 "The HTTP location of the phabricator instance.  This is only used for
 differentials that cannot be understood via the database.  The raw diffs
 for these are pulled from the instance, and so they need to be accessible
 from the script.")

; The local filesystem storage location
(defvar *storage-location* nil
 "The path on the local file system for the local storage of files.  The phabricator_file
 database can refer to local storage items, which are stored here (other options being
 that the file is stored in the database).")

(defvar *working-directory* (format nil "~Aphabricator" forgerie-core:*working-directory*))

; A list of plists, each having the keys :key and :repository
; For each of these, the project at key :key will be assigned to, and only to, repository :repository
(defvar *project-assignment-overrides* nil
 "A list of plists of override commands for projects.  Each item in the list is of the form
 '(:key KEY :repository SLUG)

 Where the KEY is the database id of the project, and the SLUG is which repository that this
 project should be a primary project of.  Then the project will be removed from all other
 repositories it mgiht be assigned to.  The ramification of this is that tasks that are
 part of this project, for instance, will be assigned to the repository in various forgeries
 that link project and repository.")
(defvar *repository-overrides* nil
 "A list of plists for overriding certain features of projects.  The plists are of the form
 '(:key KEY :action ACTION)

 Where KEY is the id of the database.  ACTION can be either :skip or :update.
   - :skip, the repository will be skipped (useful for things like the staging repository)
   - :update, will be require a further item :DATA which is a plist of overrides corresponding
     to database fields for the repository table.  Useful when renaming items, or specifying
     slugs.")

(defvar *user-overrides* nil
 "A list of plists for overriding certain features of users.  The plists are of the form
 '(:key KEY :action ACTION)

 Where KEY is the id of the database.  ACTION can be only :update.
   - :update, will be require a further item :DATA which is a plist of overrides corresponding
     to database fields for the user table.  Useful when renaming user names, or specifying
     other aspects of the user.")

; List of spaces for tasks that should be marked as confidential
(defvar *confidential-space-phids* nil
 "List of spaces that should be marked as confidential on the export.")

; List of repositories to process, keyed by repository slug
(defvar *included-repositories* nil
 "When doing only a partial import, use to list which slugs to be imported.

 This means that any tasks will be not mappable to a repository, and may end up in the
 default project of the exporter, so when using this, you'll want to disable that feature
 in the exporter of choice")

(defvar *staging-repository* nil
 "PHID of the staging repository, if used. If NIL, commits for differentials will
 not be extracted from staging.  Used if set up with arcanist.")

(defvar *email-address-sanitizer* nil
 "A function that takes a string representing an email address, and then returns
 one that should be used in its place.  Used in testing mode to ensure that
 users aren't getting random emails from the export forgerie, as an extra precaution.")
