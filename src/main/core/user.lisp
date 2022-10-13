(in-package #:forgerie-core)

(defstruct email address is-primary is-verified)

(defstruct user
 username
 name
 emails ; there can be multiple
 admin ; boolean for now
 avatar
 )

(defun user-primary-email (user)
 (find-if #'email-is-primary (user-emails user)))
