(in-package #:forgerie-core)

(defvar *log-mapping-errors* t)

(defgeneric display-mapping-error (error-type object-id description))

(defstruct mapping-error
 error-type
 object-id
 description)

(defvar *mapping-errors* nil)

(defun mapping-errors-file ()
 (format nil "~A/errors" *working-directory*))

(defun mapping-errors ()
 (or
  *mapping-errors*
  (setf *mapping-errors* (when (probe-file (mapping-errors-file)) (with-open-file (str (mapping-errors-file)) (read str))))))

(defun add-mapping-error (error-type object-id description)
 (when
  (and
   *log-mapping-errors*
   (not
    (find-if
     (lambda (mapping-error)
      (and
       (equal (mapping-error-error-type mapping-error) error-type)
       (equal (mapping-error-object-id mapping-error) object-id)))
     (mapping-errors))))
  (setf
   *mapping-errors*
   (cons
    (make-mapping-error
     :error-type error-type
     :object-id object-id
     :description description)
    (mapping-errors)))
  (with-open-file (str (mapping-errors-file) :direction :output :if-exists :supersede)
   (format str "~S" (mapping-errors))))
 (forgerie-core:check-for-stop))
