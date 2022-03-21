(in-package #:forgerie-core)

(defvar *working-directory* nil)

(define-condition stop-processing nil nil)

(defvar *continue-processing* t)
(defun check-for-stop ()
 (when (not *continue-processing*)
  (error (make-instance 'stop-processing))))

(defgeneric import-forge (forge))
(defgeneric export-forge (forge data))

; Files should be stored on disk somewhere
(defstruct file id name size location mimetype)

; A branch in forgerie exists outside of git branches.  Because
; we import things that exist at certain times, the branch may
; not even exist anymore in the git repository, or may be on a
; commit that doesn't make sense for the instance of the branch
; in the forgerie format.  Consider a PR in github against the
; main branch from a year ago.  We want to create that PR in the
; target forge against main as it was at that point in time.
(defstruct branch name commit)

; The parsed comment here is a comment after being parsed by the
; source systems, allowing target systems to do interesting things
; like update mappings and provide information.
(defstruct commit sha parsed-comment)
(defstruct patch diff)

; "text" here is actually a list of:
; - string - just a string
; - (:merge-request <id> <original-string>)
; - (:ticket <id> <original-string>)
; - (:snippet <id> <original-string>)
(defstruct note id text author date)
