(defpackage #:forgerie-core (:use :cl)
 (:export
  ; run.lisp
  #:run

  ; base.lisp
  #:import-forge #:export-forge
  #:make-file #:file-name #:file-data
  #:make-commit #:commit-sha #:commit
  #:make-patch #:patch-diff #:patch
  #:make-branch #:branch-name #:branch-commit
  #:make-note #:note-text #:note-author #:note-date #:note-id

  ; user.lisp
  #:make-user #:user-username #:user-name #:user-emails #:make-email #:email-address #:email-is-primary #:user-primary-email

  ; project.lisp
  #:make-project #:project-name #:project-tags

  ; ticket.lisp
  #:make-ticket #:ticket-id #:ticket-projects #:ticket-title #:ticket-notes #:ticket-author #:ticket-description #:ticket-date
  #:ticket-type

  ; vc-repository.lisp
  #:make-vc-repository #:vc-repository-name #:vc-repository-slug #:vc-repository-primary-projects #:vc-repository-projects
  #:vc-repository-git-location

  ; snippet.lisp
  #:make-snippet #:snippet-id #:snippet-title #:snippet-files #:snippet-notes #:snippet-author

  ; merge-request.lisp
  #:make-merge-request #:merge-request-id #:merge-request-vc-repository #:merge-request-title #:merge-request-description
  #:merge-request-source-branch #:merge-request-target-branch #:merge-request-changes #:merge-request-patch
  #:merge-request-type #:merge-request-notes #:merge-request-author #:merge-request-date

  #:make-merge-request-change #:merge-request-change-change #:merge-request-change-comments

  #:make-merge-request-change-comment #:merge-request-change-comment-line #:merge-request-change-comment-text
  #:merge-request-change-comment-replies #:merge-request-change-comment-author #:merge-request-change-comment-file
  #:merge-request-change-comment-date

  ; utils.lisp
  #:vc-repositories-with-primary-project #:git-cmd #:*debug*))
