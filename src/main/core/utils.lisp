(in-package #:forgerie-core)

(defun vc-repositories-with-primary-project (project vc-repositories)
 (remove-if-not
  (lambda (repo)
   (find project (vc-repository-primary-projects repo) :test #'equalp))
  vc-repositories))

