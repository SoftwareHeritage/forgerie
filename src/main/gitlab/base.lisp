(in-package #:forgerie-gitlab)

(defvar *server-address* nil)
(defvar *private-token* nil)

(defvar *working-directory* "/tmp/forgerie/gitlab/")

; This is a plist of the form:
; '(:name <name> :slug <slug>)
(defvar *default-project* nil)

(defvar *ssh-public-key* nil)

; This is of the form
; '(:name <name> :path <slug>)
(defvar *default-group* nil)

; For development only.  Will limit all exporting to things having
; to do with the project with the name provided.
(defvar *single-project* nil)
