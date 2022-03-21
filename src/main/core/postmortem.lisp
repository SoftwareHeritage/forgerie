(in-package #:forgerie-core)

(defgeneric system-postmortem (system))

(defun postmortem (&rest systems)
 (mapcar #'system-postmortem systems)
 (mapcar
  (lambda (mapping-error)
   (display-mapping-error
    (mapping-error-error-type mapping-error)
    (mapping-error-object-id mapping-error)
    (mapping-error-description mapping-error)))
  (reverse (mapping-errors))))
