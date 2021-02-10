(in-package #:forgerie-phabricator)

(defun query (query)
 (let*
  ((result (car (cl-mysql:query query)))
   (rows (car result))
   (definitions (cadr result)))
  (mapcar
   (lambda (row)
    (apply #'append
     (mapcar
      (lambda (col def)
       (list (intern (string-upcase (car def)) :keyword) col))
      row definitions)))
  rows)))

(defun initialize ()
 (cl-mysql:connect :password *database-password*)
 (cl-mysql:query "set names 'utf8'"))

; This function is only for development mode.  While we have emails
; turned off for gitlab, there's a chance that something screwed up will happen
; so we should make it so the aren't real email addresses
(defun sanitize-address (address)
 (format nil "~A@opentechstrategies.com" (cl-ppcre:regex-replace-all "@" address "_")))

(defun get-emails (user-phid)
 (query (format nil "select * from phabricator_user.user_email where userphid = '~A'" user-phid)))

(defun get-user (phid)
 (query "select username, realName from phabricator_user.user where phid = '~A'" phid))

(defun get-users ()
 (query "select username, realName, phid from phabricator_user.user"))

(defun get-project (phid)
 (first (query (format nil "select phid, name, icon from phabricator_project.project where phid = '~A'" phid))))

(defun get-projects ()
 (query "select phid, name, icon from phabricator_project.project"))

(defun get-tasks ()
 (mapcar
  (lambda (task)
   (append
    (list
     :projects
     (mapcar
      (lambda (result) (get-project (getf result :dst)))
      (query
       (format nil
        "select dst from phabricator_maniphest.edge where src = '~A' and dst like 'PHID-PROJ%'"
        (getf task :phid)))))
    task))
  (query "select * from phabricator_maniphest.maniphest_task")))

(defun get-repositories ()
 (let
  ((repositories (query "select phid, repositoryslug, name from phabricator_repository.repository")))
  (mapcar
   (lambda (repo)
    (let
     ((associated-projects
       (mapcar #'get-project
        (mapcar
         (lambda (result) (getf result :dst))
         (query
          (format nil
           "select * from phabricator_repository.edge where src = '~A' and dst like 'PHID-PROJ%'"
           (getf repo :phid)))))))
     (append
      (list :primary-projects (remove-if-not (lambda (project) (string= "folder" (getf project :icon))) associated-projects))
      (list :projects associated-projects)
      repo)))
   repositories)))

(defun get-file (file-phid)
 (let*
  ((file
    (first
     (query
      (format nil "select name, storageEngine, storageFormat, storageHandle from phabricator_file.file where phid = '~A'"
       file-phid)))))
  (append
   file
   (list
    :data
    (cond
     ((and
       (string= "blob" (getf file :storageengine))
       (string= "raw" (getf file :storageformat)))
      (map 'string #'code-char
       (getf
        (first
         (query
          (format nil "select data from phabricator_file.file_storageblob where id = '~A';"
           (getf file :storagehandle))))
        :data)))
     (t
      (error
       "Don't know how to handle files of with engine/format of ~A/~A encounted on ~A"
       (getf file :storageengine)
       (getf file :storageformat)
       file-phid)))))))

(defun get-pastes ()
 (remove
  nil
  (mapcar
   (lambda (paste)
    (let
     ; ignore-errors here is due to the nature of the data we're working with,
     ; and should probably get removed later on
     ((file (ignore-errors (get-file (getf paste :filephid)))))
     (when file (append (list :file file) paste))))
   (query "select id, title, filePHID from phabricator_paste.paste"))))

(defun convert-repository-to-core (repository-def)
 (forgerie-core:make-vc-repository
  :name (getf repository-def :name)
  :slug (getf repository-def :repositoryslug)
  :projects (mapcar #'convert-project-to-core (getf repository-def :projects))
  :primary-projects (mapcar #'convert-project-to-core (getf repository-def :primary-projects))))

(defun convert-project-to-core (project-def)
 (forgerie-core:make-project
  :name (getf project-def :name)))

(defun convert-email-to-core (email-def)
 (forgerie-core:make-email
  :address (sanitize-address (getf email-def :address))
  :is-primary (eql (getf email-def :isprimary) 1)))

(defun convert-user-to-core (user-def)
 (forgerie-core:make-user
  :username (getf user-def :username)
  :name (getf user-def :realname)
  :emails (mapcar #'convert-email-to-core (get-emails (getf user-def :phid)))))

(defun convert-task-to-core (task-def)
 (forgerie-core:make-ticket
  :id (getf task-def :id)
  :title (getf task-def :title)
  :projects (mapcar #'convert-project-to-core (getf task-def :projects))))

(defun convert-file-to-core (file-def)
 (forgerie-core:make-file
  :name (getf file-def :name)
  :data (getf file-def :data)))

(defun convert-paste-to-core (paste-def)
 (forgerie-core:make-snippet
  :id (getf paste-def :id)
  :title (getf paste-def :title)
  :files (list (convert-file-to-core (getf paste-def :file)))))

(defmethod forgerie-core:import-forge ((forge (eql :phabricator)))
 (initialize)
 (list
  :users
  (mapcar #'convert-user-to-core (get-users))
  :projects
  (mapcar #'convert-project-to-core (get-projects))
  :vc-repositories
  (mapcar #'convert-repository-to-core (get-repositories))
  :snippets
  (mapcar #'convert-paste-to-core (get-pastes))
  :tickets
  (mapcar #'convert-task-to-core (get-tasks))))
