(in-package #:forgerie-phabricator)

(defmethod forgerie-core:system-postmortem ((system (eql :phabricator)))
 (initialize)
 (format t "Doing phab's postmortem~%")
 (mapcar
  (lambda (override)
   (format t "Project '~A' assigned as the primary project to repository '~A'~%" 
    (getf override :name)
    (getf override :repository)))
  *project-assignment-overrides*)
 (mapcar
  (lambda (rev-id)
   (let
    ((rev (get-revision rev-id)))
    (format t "Revision ~A skipped due to configuration (status: ~A)~%" rev-id (differential-revision-status rev))))
  *revisions-to-skip*)
 (mapcar
  (lambda (paste-id)
   (format t "Paste ~A skipped due to configuation~%" paste-id))
  *pastes-to-skip*)
 (mapcar
  (lambda (user-override)
   (format t "User ~A updated with data ~S~%" (getf user-override :key) (getf user-override :data)))
  *user-overrides*)
 (mapcar
  (lambda (repository-override)
   (let
    ((repository (get-repository-by-id (getf repository-override :key))))
    (format t "Repository ~A ~A~%"
     (repository-name repository)
     (case (getf repository-override :action)
      (:update
       (format nil "updated with data ~A" (getf repository-override :data)))
      (:skip
       "skipped")))))
  *repository-overrides*)
 (format t "---------------------~%"))
