(in-package #:forgerie-core)

(defun vc-repositories-with-primary-project (project vc-repositories)
 (remove-if-not
  (lambda (repo)
   (find project (vc-repository-primary-projects repo) :test #'equalp))
  vc-repositories))

(defun git-cmd (git-dir cmd args &key (error t) (debug nil) (input nil))
 (let*
  ((err nil)
   (out nil)
   (code nil))
  (setf
   err
   (with-output-to-string (err-str)
    (setf out
     (with-output-to-string (out-str)
      (setf code
       (sb-ext:process-exit-code
        (sb-ext:run-program "/usr/bin/git"
         (append
          (list "-C" git-dir)
          (list cmd)
          args)
         :output out-str
         :error err-str
         :input input
         :wait t)))))))
  (if debug
   (format t "Return Code: ~A~%Standard output:~%~A~%Error output:~%~A~%" code out err))
  (if (and error (not (zerop code)))
   (error "Got error running git command ~A with args ~S in dir ~A" cmd args git-dir))
  (values code out err)))
