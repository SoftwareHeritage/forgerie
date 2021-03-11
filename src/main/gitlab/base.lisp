(in-package #:forgerie-gitlab)

(defvar *server-address* nil)
(defvar *private-token* nil)
(defvar *root-password* nil)

(defvar *working-directory* "/tmp/forgerie/gitlab/")

; This is a plist of the form:
; '(:name <name> :slug <slug>)
(defvar *default-project* nil)
