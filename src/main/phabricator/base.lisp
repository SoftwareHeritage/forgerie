(in-package #:forgerie-phabricator)

(defvar *database-password* nil)

; This needs to be some http or https that's accessible from forges that are importing.
; Usually you can just link the phabricators repositories directory to some http or https
; that the gitlab instance can access.
(defvar *git-location* nil)
