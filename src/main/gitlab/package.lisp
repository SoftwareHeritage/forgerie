(defpackage #:forgerie-gitlab (:use :cl)
 (:export
  #:*private-token* #:*server-address* #:*default-project* #:*working-directory* #:*ssh-public-key*
  #:*default-group* #:*single-project* #:*rails-console-ssh-args* #:*merge-request-suffix*
  #:*ticket-suffix* #:*limit-to-active-users* #:*omit-default-project*))
