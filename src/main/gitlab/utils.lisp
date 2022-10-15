(in-package #:forgerie-gitlab)

(define-condition http-error
 nil
 ((code :initarg :code :reader http-error-code)
  (path :initarg :path :reader http-error-path)
  (method :initarg :method :reader http-error-method)
  (parameters :initarg :parameters :reader http-error-parameters)
  (resp :initarg :resp :reader http-error-resp))
 (:report
  (lambda
   (condition stream)
   (format stream "Http error code: ~A, resp: ~A" (http-error-code condition) (http-error-resp condition)))))

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

(defun make-request (path method parameters &key sudo headers)
 (let
  ((parameters
    (append
     (when sudo (list (cons "sudo" sudo)))
     parameters)))
  (handler-case
   (multiple-value-bind
    (body code headers uri stream)
    (handler-bind ((simple-error  ;; this triggers when trying to write to a closed SSL context
                    (dex:retry-request 3
                     :interval (lambda (attempt) (ash 1 (+ attempt 2))))))
     (dex:request (format nil "~A/api/v4/~A" *server-address* path) :method method :content parameters
                  :headers `(("PRIVATE-TOKEN" . ,*private-token*) ,@headers)))
    (when
     (not (= 304 code)) ; 304s are empty, and can be ignored
     (let
      ((resp (convert-js-to-plist (jsown:parse body))))
      (when forgerie-core:*debug*
       (format t "*****************~%Gitlab request ~A (~A): ~S~%Status Code: ~S~%Response: ~S~%"
        path
        method
        parameters
        code
        resp))
      resp)))
   (dex:http-request-failed (e)
      (error
       (make-instance
        'http-error
        :code (dex:response-status e)
        :path path
        :method method
        :parameters parameters
        :resp (convert-js-to-plist (jsown:parse (dex:response-body e)))
        )))
   )))

(defun git-cmd (project cmd &rest args)
  (forgerie-core:git-cmd
   (format nil "~A~A" *working-directory* (getf project :path)) cmd args))

(defun git-cmd-code (project cmd &rest args)
 (forgerie-core:git-cmd
  (format nil "~A~A" *working-directory* (getf project :path)) cmd args :error nil))

(defun get-request (path &key parameters sudo headers)
 (make-request path :get parameters :sudo sudo :headers headers))

(defun post-request (path parameters &key sudo headers)
 (make-request path :post parameters :sudo sudo :headers headers))

(defun delete-request (path &key headers)
 (make-request path :delete nil :headers headers))

(defun put-request (path parameters &key sudo headers)
 (make-request path :put parameters :sudo sudo :headers headers))

(defun merge-request-suffix (mr)
 (if
  *merge-request-suffix*
  (funcall *merge-request-suffix* mr)
  ""))

(defun ticket-suffix (ticket)
 (if
  *ticket-suffix*
  (funcall *ticket-suffix* ticket)
  ""))

(defun fallback-file-text (file)
 (if *fallback-file-text*
  (funcall *fallback-file-text* file)
  (format nil "File <~A (id=~A)> too large, skipped upload"
   (forgerie-core:file-name file) (forgerie-core:file-id file))))

(defun namespace-for-repo (repository)
 (if
  *namespace-for-repo*
  (funcall *namespace-for-repo* repository)))

(defun to-iso-8601 (d)
 (forgerie-core:to-iso-8601 d))

(defstruct mapped-item type original-id id iid project-id)
(defstruct mapped-file type original-id response)

(defun mapping-file ()
 (format nil "~A/mapping" *working-directory*))

(defvar *mapping* nil)

(defun mapping ()
 (or
  *mapping*
  (setf *mapping*
   (when
    (probe-file (mapping-file))
    (with-open-file (str (mapping-file))
     (loop :for obj := (read str nil)
      :while obj
      :collect obj))))))

(defun find-mapped-item (type original-id)
 (find
  (list type original-id)
  (mapping)
  :key
  (lambda (mi)
   (typecase mi
    (mapped-item (list (mapped-item-type mi) (mapped-item-original-id mi)))
    (mapped-file (list (mapped-file-type mi) (mapped-file-original-id mi)))))
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
   (str (gensym))
   (mapped-item (gensym)))
 `(let*
   ((,result ,@body)
    (,mapped-item
     (make-mapped-item
      :type ,type
      :original-id ,original-id
      :id (getf ,result :id)
      :iid (getf ,result :iid)
      :project-id (getf ,result :project_id))))
   (setf
    *mapping*
    (cons
     ,mapped-item
     (mapping)))
   (with-open-file (,str (mapping-file) :direction :output :if-exists :append :if-does-not-exist :create)
    (format ,str "~S~%" ,mapped-item))
   (forgerie-core:check-for-stop)
   ,result)))

(defmacro update-file-mapping ((type original-id) &rest body)
 (let
  ((result (gensym))
   (str (gensym))
   (mapped-item (gensym)))
 `(let*
   ((,result ,@body)
    (,mapped-item
     (make-mapped-file
      :type ,type
      :original-id ,original-id
      :response ,result)))
   (setf
    *mapping*
    (cons ,mapped-item (mapping)))
   (with-open-file (,str (mapping-file) :direction :output :if-exists :append :if-does-not-exist :create)
    (format ,str "~S~%" ,mapped-item))
   (forgerie-core:check-for-stop)
   ,result)))

(defun retrieve-mapping (type original-id)
 (let
  ((mi (find-mapped-item type original-id)))
  (when (not mi)
   (error "Failed to retrieve mapping for ~S" (list type original-id)))
  (typecase mi
   (mapped-item
    (if (mapped-item-project-id mi)
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
     (get-request
      (format nil "~A/~A"
       (case (mapped-item-type mi)
        (:user "users"))
       (or
        (mapped-item-iid mi)
        (mapped-item-id mi))))))
    (mapped-file (mapped-file-response mi)))))

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
   (sb-ext:run-program *rails-command* *rails-command-args*
    :input :stream
    :output :stream
    :wait nil))
  (format (sb-ext:process-input *rails-connection*) "0~%")
  (force-output (sb-ext:process-input *rails-connection*))
  (loop for line = (read-line (sb-ext:process-output *rails-connection*))
        do (when forgerie-core:*debug* (format t "Booting rails-console: ~A~%" line))
        until (string= line "0")))
 ; The reason we append a 0 on the end of this, is because irb does some funky
 ; things, expecting you to be running from a terminal with a tty.  So just
 ; doing a 0 and then checking for that output means we'll A) know when the
 ; command is done and B) not run into these no tty errors.
 (when forgerie-core:*debug* (format t "Sending rails-console command ~A;0~%" cmd))
 (format (sb-ext:process-input *rails-connection*) "~A;0~%" cmd)
 (force-output (sb-ext:process-input *rails-connection*))
 (let
  ((line (read-line (sb-ext:process-output *rails-connection*))))
  (loop for line = (read-line (sb-ext:process-output *rails-connection*))
        do (when forgerie-core:*debug* (format t "Output: ~A~%" line))
        until (string= line "0"))))

(defun rails-commands-with-recovery (commands)
 (block nil
  (let ((times 0))
   (tagbody
    retry
     (handler-case
      (progn
       (mapc #'rails-command commands)
       (return nil))
      ((or sb-int:broken-pipe end-of-file)
       (condition)
       (format t "Got error ~A when running rails commands~%" condition)
       (when (= times 2)
        (error "Failed to run rails commands ~A times: ~A~%" (+ times 1) commands))
       (incf times)
       (let ((nsecs (ash 1 (+ times 1))))  ;; 4 8 16
        (format t "Sleeping for ~As~%" nsecs)
        (sleep nsecs)
        (sb-ext:process-close *rails-connection*)
        (if (sb-ext:process-alive-p *rails-connection*)
         (sb-ext:process-kill *rails-connection* 9))
        (setf *rails-connection* nil)
        (go retry))))))))

(defun rails-wait-for (target cmd)
 (format nil "~A = ~A; begin sleep(0.1); ~A = ~A end while !~A" target cmd target cmd target))
