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

(defun delete-request (path)
 (make-request path :delete nil))

(defun put-request (path parameters &key sudo)
 (make-request path :put parameters :sudo sudo))

(defun to-iso-8601 (d)
 (multiple-value-bind (sec min hr date month year) (decode-universal-time d 0)
  (format nil "~A-~2,,,'0@A-~2,,,'0@AT~2,,,'0@A:~2,,,'0@A:~2,,,'0@AZ" year month date hr min sec)))

(defstruct mapped-item type original-id id iid project-id)

(defun mapping-file ()
 (format nil "~A/mapping" *working-directory*))

(defvar *mapping* nil)

(defun mapping ()
 (or
  *mapping*
  (setf *mapping* (when (probe-file (mapping-file)) (with-open-file (str (mapping-file)) (read str))))))

(defun find-mapped-item (type original-id)
 (find
  (list type original-id)
  (mapping)
  :key
  (lambda (mi)
   (list
    (mapped-item-type mi)
    (mapped-item-original-id mi)))
  :test #'equalp))

(defmacro when-unmapped ((type original-id) &rest body)
 `(when (not (find-mapped-item ,type ,original-id)) ,@body))

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
     (make-mapped-item
      :type ,type
      :original-id ,original-id
      :id (getf ,result :id)
      :iid (getf ,result :iid)
      :project-id (getf ,result :project_id))
     (mapping)))
   (with-open-file (,str (mapping-file) :direction :output :if-exists :supersede)
    (format ,str "~S" (mapping)))
   ,result)))

(defun retrieve-mapping (type original-id)
 (let
  ((mi (find-mapped-item type original-id)))
  (if mi
   (get-request
    (format
     nil
     "projects/~A/~A/~A"
     (mapped-item-project-id mi)
     (case (mapped-item-type mi)
      (:snippet "snippets")
      (:merge-request "merge_requests")
      (:ticket "issues"))
     (or
      (mapped-item-iid mi)
      (mapped-item-id mi))))
   (error "Failed to retrieve mapping for ~S" (list type original-id)))))

; This is for development, so that we can export only one project
; and all the tickets/prs associated with it.
(defmacro single-project-check (name &rest body)
 `(when
   (or
    (not *single-project*)
    (string= *single-project* ,name))
   ,@body))

(defvar *rails-connection* nil)

; Each command needs to be a one liner standalone
(defun rails-command (cmd)
 (when (not *rails-connection*)
  (setf
   *rails-connection*
   (sb-ext:run-program "/usr/bin/ssh" *rails-console-ssh-args*
    :input :stream
    :output :stream
    :wait nil))
  (format (sb-ext:process-input *rails-connection*) "0~%" cmd)
  (force-output (sb-ext:process-input *rails-connection*))
  (loop for line = (read-line (sb-ext:process-output *rails-connection*))
        do (format t "Booting: ~A~%" line)
        until (string= line "0")))
 ; The reason we append a 0 on the end of this, is because irb does some funky
 ; things, expecting you to be running from a terminal with a tty.  So just
 ; doing a 0 and then checking for that output means we'll A) know when the
 ; command is done and B) not run into these no tty errors.
 (format (sb-ext:process-input *rails-connection*) "~A;0~%" cmd)
 (force-output (sb-ext:process-input *rails-connection*))
 (let
  ((line (read-line (sb-ext:process-output *rails-connection*))))
  (loop for line = (read-line (sb-ext:process-output *rails-connection*))
        do (when forgerie-core:*debug* (format t "Running: ~A~%" line))
        until (string= line "0"))))
