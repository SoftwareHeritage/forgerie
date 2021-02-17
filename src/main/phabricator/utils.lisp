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
          (format nil "~{#~A~^~%~}" (getf proj :tags))
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

(defun unix-to-universal-time (d)
 (+ d (encode-universal-time 0 0 0 1 1 1970 0)))
