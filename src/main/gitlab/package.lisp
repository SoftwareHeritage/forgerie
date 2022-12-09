(defpackage #:forgerie-gitlab (:use :cl)
 (:export
  #:*private-token* #:*server-address* #:*default-project* #:*ssh-public-key*
  #:*default-group* #:*group-structure* #:*single-project* #:*rails-command* #:*rails-command-args*
  #:*namespace-for-repo* #:*merge-request-suffix* #:*ticket-suffix* #:*limit-to-active-users*
  #:*always-migrate-users*
  #:*snippet-suffix* #:*fallback-file-text* #:*fallback-mapped-item-text* #:*write-completed-mappings*))
