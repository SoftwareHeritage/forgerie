(in-package #:forgerie-core)

(defgeneric import-forge (forge))
(defgeneric export-forge (forge data))

(defstruct file name data)

; A branch in forgerie exists outside of git branches.  Because
; we import things that exist at certain times, the branch may
; not even exist anymore in the git repository, or may be on a
; commit that doesn't make sense for the instance of the branch
; in the forgerie format.  Consider a PR in github against the
; main branch from a year ago.  We want to create that PR in the
; target forge against main as it was at that point in time.
(defstruct branch name commit)

(defstruct commit sha)
(defstruct patch diff)

(defstruct note text author)
