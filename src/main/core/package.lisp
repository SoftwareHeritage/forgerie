(defpackage #:forgerie-core (:use :cl)
 (:export
  ; base.lisp
  #:import-forge #:export-forge

  ; run.lisp
  #:run

  ; user.lisp
  #:make-user #:user-username #:user-name #:user-emails #:make-email #:email-address #:email-is-primary #:user-primary-email

  ; project.lisp
  #:make-project #:project-name

  ; ticket.lisp
  #:make-ticket

  ; vc-repository.lisp
  #:make-vc-repository #:vc-repository-name #:vc-repository-slug #:vc-repository-primary-projects #:vc-repository-projects

  ; utils.lisp
  #:vc-repositories-with-primary-project))
