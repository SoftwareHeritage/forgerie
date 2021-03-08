(in-package #:forgerie-gitlab)

(defun convert-js-to-plist (jsown)
 (cond
  ((not (listp jsown)) jsown)
  ((eql :obj (car jsown))
   (apply #'append
    (mapcar
     (lambda (keyword)
      (list
       (intern (string-upcase keyword) :keyword)
       (convert-js-to-plist (jsown:val jsown keyword))))
     (jsown:keywords jsown))))
  ((listp jsown)
   (mapcar #'convert-js-to-plist jsown))
  (t (error "Don't know how to handle ~S" jsown))))

(defun make-request (path method parameters &key sudo)
 (let
  ((parameters
    (cons
     (cons "private_token" *private-token*)
     (append
      (when sudo (list (cons "sudo" sudo)))
      parameters))))
  (convert-js-to-plist
   (jsown:parse
    (map 'string #'code-char
     (drakma:http-request
      (format nil "~A/api/v4/~A" *server-address* path)
      :method method
      :parameters parameters))))))

(defun git-cmd (project cmd &rest args)
 (forgerie-core:git-cmd
  (format nil "~A~A" *checkout-path* (getf project :path)) cmd args))

(defun get-request (path &key parameters sudo)
 (make-request path :get parameters :sudo sudo))

(defun post-request (path parameters &key sudo)
 (make-request path :post parameters :sudo sudo))

(defun put-request (path parameters &key sudo)
 (make-request path :put parameters :sudo sudo))

(defun to-iso-8601 (d)
 (multiple-value-bind (sec min hr date month year) (decode-universal-time d 0)
  (format nil "~A-~2,,,'0@A-~2,,,'0@AT~2,,,'0@A:~2,,,'0@A:~2,,,'0@AZ" year month date hr min sec)))

