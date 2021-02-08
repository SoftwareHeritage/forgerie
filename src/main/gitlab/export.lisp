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

(defmethod forgerie-core:export-forge ((forge (eql :gitlab)) data)
 (mapcar #'create-user (getf data :users)))

(defun create-user (user)
 (post-request
  "users"
  `(("name" . ,(forgerie-core:user-name user))
    ("email" . ,(forgerie-core:email-address (forgerie-core:user-primary-email user)))
    ("reset_password" . "true")
    ("username" . ,(forgerie-core:user-username user)))))
