(in-package #:forgerie-core)

(defvar *log-mapping-errors* t
 "Boolean which indicates whether mapping errors should be logged.  Defaults
 to T to create a mapping errors file that can be later used to output
 what errors the run ran into.")

(defgeneric display-mapping-error (error-type object-id description))

(defstruct mapping-error
 error-type
 object-id
 description)

(defvar *mapping-errors* nil)

(defun mapping-errors-directory ()
  (format nil "~Acore/" *working-directory*))

(defun mapping-errors-file ()
  (let
      ((dir (mapping-errors-directory)))
    (ensure-directories-exist dir)
    (format nil "~Aerrors" dir)))

(defun mapping-errors ()
 (or
  *mapping-errors*
  (setf *mapping-errors*
   (when
    (probe-file (mapping-errors-file))
     (with-open-file (str (mapping-errors-file))
      (loop :for obj := (read str nil)
       :while obj
       :collect obj))))))

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
  (let
   ((mapping-error
     (make-mapping-error
      :error-type error-type
      :object-id object-id
      :description description)))
   (setf
    *mapping-errors*
    (cons
     mapping-error
     (mapping-errors)))
   (with-open-file (str (mapping-errors-file) :direction :output :if-exists :append :if-does-not-exist :create)
    (format str "~S~%" mapping-error)))
  (forgerie-core:check-for-stop)))
