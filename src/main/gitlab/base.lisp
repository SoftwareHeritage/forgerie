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

; The args (host and command are normal) for the ssh command to
; boot the rails console.  Sometimes this is localhost.  Keys
; have to be set up.
(defvar *rails-console-ssh-args* nil)

; A funciton that takes a forgerie-core:merge-request and adds a string
; that should be appended to the description of merge requests.
(defvar *merge-request-suffix* nil)
