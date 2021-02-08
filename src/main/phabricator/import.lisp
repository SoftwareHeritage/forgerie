(in-package #:forgerie-phabricator)

(defun query (query)
 (let*
  ((result (car (cl-mysql:query query)))
   (rows (car result))
   (definitions (cadr result)))
  (mapcar
   (lambda (row)
    (apply #'append
     (mapcar
      (lambda (col def)
       (list (intern (string-upcase (car def)) :keyword) col))
      row definitions)))
  rows)))

(defun initialize ()
 (cl-mysql:connect :password *database-password*)
 (cl-mysql:query "set names 'utf8'"))

; This function is only for development mode.  While we have emails
; turned off for gitlab, there's a chance that something screwed up will happen
; so we should make it so the aren't real email addresses
(defun sanitize-address (address)
 (format nil "~A@opentechstrategies.com" (cl-ppcre:regex-replace-all "@" address "_")))

(defun get-emails (user-phid)
 (let
  ((result (query (format nil "select * from user_email where userphid = '~A'" user-phid))))
  (mapcar 
   (lambda (result)
    (forgerie-core:make-email
     :address (sanitize-address (getf result :address))
     :is-primary (eql (getf result :isprimary) 1)))
   result)))

(defun get-users ()
 (cl-mysql:use "phabricator_user")
 (mapcar
  (lambda (user-def)
   (let
    ((user (forgerie-core:make-user)))
    (setf (forgerie-core:user-username user) (getf user-def :username))
    (setf (forgerie-core:user-name user) (getf user-def :realname))
    (setf (forgerie-core:user-emails user) (get-emails (getf user-def :phid)))
    user))
  (query "select phid, username, realName from user")))

(defmethod forgerie-core:import-forge ((forge (eql :phabricator)))
 (initialize)
 (list
  :users
  (get-users)))
