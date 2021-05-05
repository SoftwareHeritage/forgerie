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
  (multiple-value-bind
   (body code headers uri stream must-close reason-phrase)
   (drakma:http-request (format nil "~A/api/v4/~A" *server-address* path) :method method :parameters parameters)
   (let
    ((resp (convert-js-to-plist (jsown:parse (map 'string #'code-char body)))))
    (when forgerie-core:*debug*
     (format t "*****************~%Gitlab request ~A (~A): ~S~%Status Code: ~S~%Response: ~S~%"
      path
      method
      parameters
      code
      resp))
    (when (not (<= 200 code 299))
     (error "Got a non-2XX code ~A when doing request ~S (~A) with parameters ~S.~%Response: ~S"
      code
      path
      method
      parameters
      resp))
    resp))))

(defun git-cmd (project cmd &rest args)
 (forgerie-core:git-cmd
  (format nil "~A~A" *working-directory* (getf project :path)) cmd args))

(defun git-cmd-code (project cmd &rest args)
 (forgerie-core:git-cmd
  (format nil "~A~A" *working-directory* (getf project :path)) cmd args :error nil))

(defun get-request (path &key parameters sudo)
 (make-request path :get parameters :sudo sudo))

(defun post-request (path parameters &key sudo)
 (make-request path :post parameters :sudo sudo))

(defun put-request (path parameters &key sudo)
 (make-request path :put parameters :sudo sudo))

(defun to-iso-8601 (d)
 (multiple-value-bind (sec min hr date month year) (decode-universal-time d 0)
  (format nil "~A-~2,,,'0@A-~2,,,'0@AT~2,,,'0@A:~2,,,'0@A:~2,,,'0@AZ" year month date hr min sec)))

(defun mapping-file ()
 (format nil "~A/mapping" *working-directory*))

(defvar *mapping* nil)

(defun mapping ()
 (or
  *mapping*
  (setf *mapping* (when (probe-file (mapping-file)) (with-open-file (str (mapping-file)) (read str))))))

(defmacro when-unmapped ((type original-id) &rest body)
 `(when
   (not (find (list ,type ,original-id) (mapping) :key #'car :test #'equalp))
   ,@body))

(defmacro when-unmapped-with-update ((type original-id) &rest body)
 `(when-unmapped (,type ,original-id)
   (update-mapping (,type ,original-id)
    ,@body)))

(defmacro update-mapping ((type original-id) &rest body)
 (let
  ((result (gensym))
   (str (gensym)))
 `(let
   ((,result ,@body))
   (setf
    *mapping*
    (cons
     (cons (list ,type ,original-id) (getf ,result :id))
     (mapping)))
   (with-open-file (,str (mapping-file) :direction :output :if-exists :supersede)
    (format ,str "~S" (mapping)))
   ,result)))
