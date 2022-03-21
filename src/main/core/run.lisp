(in-package #:forgerie-core)

(defun run (forge-for-import forge-for-export)
 (let*
  ((m (sb-thread:make-mutex))
   (q (sb-thread:make-waitqueue))
   (running-thread
    (sb-thread:make-thread
     (lambda ()
      (handler-case
       (export-forge
        forge-for-export
        (import-forge
          forge-for-import))
       (stop-processing (e)
        (format t "We've errored out due to stop processing!~%")))
      (sb-thread:with-mutex (m)
       (sb-thread:condition-broadcast q))))))
  (push
   (lambda ()
    (format t "Shutting down....~%")
    (setf *continue-processing* nil)
    (sb-thread:join-thread running-thread))
   sb-ext:*exit-hooks*)
  (sb-thread:with-mutex (m)
   (sb-thread:condition-wait q m))))
