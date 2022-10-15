(defpackage #:forgerie-core (:use :cl)
 (:export
  ; run.lisp
  #:run

  ; postmortem.lisp
  #:postmortem #:system-postmortem

  ; base.lisp
  #:*working-directory* #:*continue-processing* #:check-for-stop
  #:import-forge #:export-forge
  #:make-file #:file-id #:file-name #:file-mimetype #:file-location #:file-size
  #:make-commit #:commit-sha #:commit #:commit-parsed-comment
  #:make-patch #:patch-diff #:patch
  #:make-branch #:branch-name #:branch-commit
  #:make-note #:note-text #:note-author #:note-date #:note-id #:note

  ; user.lisp
  #:make-user #:user-username #:user-name #:user-emails #:make-email #:email-address #:email-is-primary #:email-is-verified
  #:user-primary-email #:user-admin #:user-avatar

  ; project.lisp
  #:make-project #:project-name #:project-tags

  ; ticket.lisp
  #:make-ticket #:ticket-id #:ticket-projects #:ticket-title #:ticket-notes #:ticket-author #:ticket-description #:ticket-date
  #:ticket-type #:ticket #:ticket-priority #:ticket-assignee #:ticket-confidential #:ticket-linked-tickets #:ticket-subscribers #:ticket-actions

  #:make-ticket-action #:ticket-action-id #:ticket-action-date #:ticket-action-author #:ticket-action-type #:ticket-action-newvalue #:ticket-action

  ; vc-repository.lisp
  #:make-vc-repository #:vc-repository-name #:vc-repository-slug #:vc-repository-primary-projects #:vc-repository-projects
  #:vc-repository-git-location #:vc-repository-commits #:vc-repository-private

  ; snippet.lisp
  #:make-snippet #:snippet-id #:snippet-title #:snippet-files #:snippet-notes #:snippet-author #:snippet-date #:snippet-private #:snippet

  ; merge-request.lisp
  #:make-merge-request #:merge-request-id #:merge-request-vc-repository #:merge-request-title #:merge-request-description
  #:merge-request-source-branch #:merge-request-target-branch #:merge-request-changes #:merge-request-patch
  #:merge-request-type #:merge-request-notes #:merge-request-author #:merge-request-date #:merge-request-actions #:merge-request

  #:make-merge-request-change #:merge-request-change-change #:merge-request-change-comments

  #:make-merge-request-change-comment #:merge-request-change-comment-new-line #:merge-request-change-comment-old-line
  #:merge-request-change-comment-text #:merge-request-change-comment-replies #:merge-request-change-comment-author
  #:merge-request-change-comment-file #:merge-request-change-comment-date #:merge-request-other-change-comments

  #:make-merge-request-action #:merge-request-action #:merge-request-action-id #:merge-request-action-author
  #:merge-request-action-date #:merge-request-action-type


  ; errors.lisp
  #:add-mapping-error #:*log-mapping-errors* #:display-mapping-error

  ; utils.lisp
  #:vc-repositories-with-primary-project #:git-cmd #:*debug* #:to-iso-8601))
