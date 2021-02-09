(in-package #:forgerie-gitlab)

(defun make-request (path method parameters)
 (let
  ((parameters
    (cons
     (cons "private_token" *private-token*)
     parameters)))
  (jsown:parse
   (map 'string #'code-char
    (drakma:http-request
     (format nil "~A/api/v4/~A" *server-address* path)
     :method method
     :parameters parameters)))))

(defun get-request (path &optional parameters)
 (make-request path :get parameters))

(defun post-request (path parameters)
 (make-request path :post parameters))

(defun validate-vc-repositories (vc-repositories projects)
 (every
  #'identity
  (list
   (every #'identity
    (mapcar
     (lambda (vcr)
      (cond
       ((not (forgerie-core:vc-repository-primary-projects vcr))
        (format *error-output* "VC Repository '~A' has no primary projects.~%" (forgerie-core:vc-repository-name vcr))
        nil)
       (t)))
     vc-repositories))
   (every #'identity
    (mapcar
     (lambda (proj)
      (let
       ((repos-for-proj (forgerie-core:vc-repositories-with-primary-project proj vc-repositories)))
       (cond
        ((< 1 (length repos-for-proj))
         (format *error-output*
          "Project ~A is the primary project in multiple repositories:~%~{ * ~A~%~}"
          (forgerie-core:project-name proj)
          (mapcar #'forgerie-core:vc-repository-name repos-for-proj))
         nil)
        (t))))
     projects)))))

; This assumes that validate-vc-repositories passed, which is to say
; that every project of interest belongs to only one repository, and that
; every vc-repository has at least one primary project
(defun validate-tickets (tickets vc-repositories)
 (flet
  ((ticket-assignable-vc-repositories (ticket)
    (when (forgerie-core:ticket-projects ticket)
     (remove
      nil
      (remove-duplicates
       (apply #'append
        (mapcar
         (lambda (proj) (forgerie-core:vc-repositories-with-primary-project proj vc-repositories))
         (forgerie-core:ticket-projects ticket)))
       :test #'equalp)))))
  (every
   #'identity
   (mapcar
    (lambda (ticket)
     (let
      ((vc-repos (ticket-assignable-vc-repositories ticket)))
      (cond
       ((not vc-repos)
        (format *error-output* "Ticket with id ~A is not assignable to a repository~%" (forgerie-core:ticket-id ticket)))
       ((< 1 (length vc-repos))
        (format *error-output*
         "Ticket with id ~A is assignable to multiple repositories:~%~{ * ~A~%~}"
         (forgerie-core:ticket-id ticket)
         (mapcar #'forgerie-core:vc-repository-name vc-repos)))
       (t))))
    tickets))))

(defmethod forgerie-core:export-forge ((forge (eql :gitlab)) data)
 (if
  (and
   (validate-vc-repositories (getf data :vc-repsitories) (getf data :projects))
   (validate-tickets (getf data :vc-repsitories) (getf data :projects)))
  (mapcar #'create-user (getf data :users))
  (format *error-output* "Unable to export to gitlab, validation failed.~%")))

(defun create-user (user)
 (post-request
  "users"
  `(("name" . ,(forgerie-core:user-name user))
    ("email" . ,(forgerie-core:email-address (forgerie-core:user-primary-email user)))
    ("reset_password" . "true")
    ("username" . ,(forgerie-core:user-username user)))))
