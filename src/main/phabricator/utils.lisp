(in-package #:forgerie-phabricator)

(defun generate-override-csv-template (output-file)
 (let
  ((projects (get-projects))
   (repositories (get-repositories)))
  (with-open-file (out output-file :direction :output :if-exists :supersede)
   (cl-csv:write-csv
    (cons
     (list
      "Primary Key"
      "Project Name"
      "Icon"
      "Color"
      "HashTags"
      "Repositories"
      "Repository Name")
     (append
      (mapcar
       (lambda (proj)
        (let
         ((associated-repos
           (remove-if-not
            (lambda (repo) (find proj (getf repo :primary-projects) :test #'equalp))
            repositories)))
         (list
          (getf proj :id)
          (getf proj :name)
          (getf proj :icon)
          (getf proj :color)
          (format nil "~{#~A~^~%~}" (project-tags proj))
          (format nil "~{~A~^~%~}" (mapcar (lambda (repo) (getf repo :repositoryslug)) associated-repos))
          (if
           (= 1 (length associated-repos))
           (getf (first associated-repos) :name)))))
       projects)
      (mapcar
       (lambda (repo)
        (list
         ""
         ""
         ""
         ""
         ""
         (getf repo :repositoryslug)
         (getf repo :name)))
       (remove nil repositories :test-not #'eql :key (lambda (repo) (getf repo :primary-projects))))))
   :stream out))))

(defun generate-task-overide-csv (ouput-file)
 (labels
  ((repositories-with-primary-project (project repositories)
    (remove-if-not
     (lambda (repo)
      (find project (repository-primary-projects repo) :test #'equalp))
     repositories)))
   (task-assignable-repositories (task repositories)
    (when (task-projects task)
     (remove
      nil
      (remove-duplicates
       (apply #'append
        (mapcar
         (lambda (proj) (repositories-with-primary-project proj repositories))
         (task-projects task)))
       :test #'equalp))))
   (build-row (task reason)
    (list
     (task-id task)
     (format nil "https://forge.softwareheritage.org/T~A" (task-id task))
     (task-title task)
     (task-status task)
     (user-username (task-owner task))
     (format nil "~{#~A~^%~}" (apply #'append (mapcar (lambda (proj) (project-tags proj)) (task-projects))))

(defun validate-tasks ()
 (let
  ((tasks (get-tasks))
   (repositories (get-repositories)))
  (with-output-to-string (out)
   (remove nil
    (cl-csv:write-csv
     (cons
      (list
       "Primary Key"
       "URL"
       "Title"
       "Status"
       "Assignee"
       "Tags"
       "Reason")
      (remove nil
       (mapcar
        (lambda (task)
         (let
          ((repos (task-assignable-repositories task repositories)))
          (cond
           ((not repos) (format t "Task with id ~A is not assignable to a repository~%" (task-id task)))
           ((< 1 (length repos))
            (format t
             "Task with id ~A is assignable to multiple repositories:~%~{ * ~A~%~}"
             (task-id task)
             (mapcar #'repository-name repos))))))
        tasks)))

(defun unix-to-universal-time (d)
 (+ d (encode-universal-time 0 0 0 1 1 1970 0)))

(defvar *override-cache* nil)

(defmacro cached (dir id item)
 (let
  ((revision-path (gensym))
   (obj (gensym)))
 `(let
   ((,revision-path (format nil "~A/~A/~A" *working-directory* ,dir ,id)))
   (ensure-directories-exist ,revision-path)
   (if
    (and (probe-file ,revision-path) (not *override-cache*))
    (with-open-file (str ,revision-path) (read str))
    (let
     ((,obj ,item))
     (with-open-file (str ,revision-path :direction :output :if-exists :supersede)
      (format str "~S" ,obj))
     ,obj)))))
