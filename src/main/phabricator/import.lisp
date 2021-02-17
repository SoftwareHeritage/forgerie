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

(defun fill-out-project (proj)
 (append
  (list
   :tags
   (mapcar
    (lambda (rslt) (getf rslt :slug))
    (query (format nil "select slug from phabricator_project.project_slug where projectphid = '~A'" (getf proj :phid)))))
  proj))

(defun get-project (phid)
 (fill-out-project
  (first
   (query
    (format nil "select id, phid, color, name, icon from phabricator_project.project where phid = '~A'" phid)))))

(defun get-projects ()
 (mapcar #'fill-out-project (query "select id, phid, color, name, icon from phabricator_project.project")))

(defun get-tasks ()
 (mapcar
  (lambda (task)
   (append
    task
    (list
     :projects
     (mapcar
      (lambda (result) (get-project (getf result :dst)))
      (query
       (format nil
        "select dst from phabricator_maniphest.edge where src = '~A' and dst like 'PHID-PROJ%'"
        (getf task :phid)))))))
  (query "select * from phabricator_maniphest.maniphest_task")))

(defun attach-projects-to-repository (repo)
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
   repo
   (list :primary-projects (remove-if-not (lambda (project) (string= "folder" (getf project :icon))) associated-projects))
   (list :projects associated-projects))))

(defun get-repository (phid)
 (attach-projects-to-repository
  (first
   (query
    (format nil
     "select id, phid, repositoryslug, name, localpath from phabricator_repository.repository where phid = '~A'"
     phid)))))

(defun get-repository-by-slug (slug)
 (attach-projects-to-repository
  (first
   (query
    (format nil
     "select id, phid, repositoryslug, name, localpath from phabricator_repository.repository where repositoryslug = '~A'"
     slug)))))

(defun get-repositories ()
 (let
  ((repositories (query "select phid, repositoryslug, name, localpath from phabricator_repository.repository")))
  (mapcar #'attach-projects-to-repository repositories)))

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

(defun get-commit (phid &optional (with-parents t))
 (let
  ((commit
    (first
     (query
      (format nil
       "select id, repositoryid, commitidentifier from phabricator_repository.repository_commit where phid = '~A'"
       phid)))))
  (append
   commit
   (list :parents
    (if with-parents
     (mapcar
      (lambda (parent-phid) (get-commit parent-phid nil))
      (mapcar
       (lambda (ref) (getf ref :phid))
       (query
        (format nil
         "select phid
             from phabricator_repository.repository_parents rp
             join phabricator_repository.repository_commit rc on rp.parentcommitid = rc.id
             where childcommitid = '~A'"
         (getf commit :id)))))
     :unfetched)))))

(defun order-related-commits (commits)
 (when (find-if (lambda (commit) (< 1 (length (getf commit :parents)))) commits)
  (error "There's a merge commit in the differential commit list?!  Investigate further"))
 (cond
  ((not commits) nil)
  ((= 1 (length commits)) commits)
  (t
   (let*
    ((parents (apply #'append (mapcar (lambda (commit) (getf commit :parents)) commits)))
     (non-parent-commits
      (remove-if
       (lambda (commit)
        (find (getf commit :commitidentifier) parents :key (lambda (parent) (getf parent :commitidentifier)) :test #'string=))
       commits)))
    (when (< 1 (length non-parent-commits))
     (format t "~S~%" non-parent-commits)
     (error "There's multiple commits that are not a parent in the set, meaning this commit chain is weird"))
    (cons
     (car non-parent-commits)
     (order-related-commits (remove (car non-parent-commits) commits)))))))

(defun get-commits-from-db (revision)
 (let
  ((repository (get-repository (getf revision :repositoryphid))))
  (order-related-commits
   (remove-if
    (lambda (commit)
     (or
      (not (eql (getf commit :repositoryid) (getf repository :id)))
      ; Is this commit reachable?
      (not
       (zerop
        (sb-ext:process-exit-code
         (sb-ext:run-program "/usr/bin/git"
          (list
           (format nil "--git-dir=~A" (getf repository :localpath))
           "cat-file"
           "-t"
           (getf commit :commitidentifier))
          :wait t))))
      (string=
       (format nil "undefined~%")
       (with-output-to-string (out)
        (sb-ext:process-exit-code
         (sb-ext:run-program "/usr/bin/git"
          (list
           (format nil "--git-dir=~A" (getf repository :localpath))
           "name-rev"
           "--name-only"
           (getf commit :commitidentifier))
          :wait t
          :output out))))
      ; Remove merge commits
      (< 1 (length (getf commit :parents)))))
    (mapcar #'get-commit
     (mapcar
      (lambda (edge) (getf edge :dst))
      ; type of 31 is the same as DifferentialRevisionHasCommitEdgeType
      (query (format nil "select dst from phabricator_differential.edge where src = '~A' and type = 31" (getf revision :phid)))))))))

(defvar *sha-detail-cache* nil)

(defun get-details (repository sha)
 (with-output-to-string (out)
  (sb-ext:run-program (asdf:system-relative-pathname :forgerie "bin/getdetails.sh")
  (list sha (getf repository :localpath))
  :wait t
  :output out)))

(defun get-shas-and-details (repository)
 (when
  (not (assoc (getf repository :phid) *sha-detail-cache* :test #'string=))
  (setf
   *sha-detail-cache*
   (cons
    (cons
     (getf repository :phid)
     (mapcar
      (lambda (sha) (list sha (get-details repository sha)))
      (cl-ppcre:split
       "\\n"
       (with-output-to-string (out)
        (sb-ext:run-program "/usr/bin/git"
         (list
          (format nil "--git-dir=~A" (getf repository :localpath))
          "log"
          "--all"
          "--pretty=%H")
         :wait t
         :output out)))))
    *sha-detail-cache*)))
 (cdr (assoc (getf repository :phid) *sha-detail-cache* :test #'string=)))

(defun get-commits-from-staging (revision)
 (let*
  ((staging-repository (get-repository "PHID-REPO-cuxcaqw5u7vepi4b4bpg"))
   (repository (get-repository (getf revision :repositoryphid)))
   (latest-diff
    (first
     (query
      (format nil "select id from phabricator_differential.differential_diff where revisionid = '~A' order by id desc limit 1"
       (getf revision :id)))))
   (all-shas-and-details (get-shas-and-details repository)))
  (labels
   ((build-commit-chain (diff-id &optional (n 0))
     (when
      (> n 20)
      (error "We have failed to find a matching commit in the previous 20"))
     (let*
      ((diff-details (get-details staging-repository (format nil "phabricator/diff/~A~~~A" diff-id n)))
       (repo-details (find diff-details all-shas-and-details :test #'string= :key #'cadr)))
      (if repo-details
       (list :commitidentifier (car repo-details) :repository repository)
       (cons
        (list
         :patch
         (with-output-to-string (out)
          (sb-ext:run-program "/usr/bin/git"
           (list
            (format nil "--git-dir=~A" (getf staging-repository :localpath))
            "format-patch"
            "-k"
            "-1"
            "--stdout"
            (format nil "phabricator/diff/~A~~~A" diff-id n))
           :wait t
           :output out)))
       (build-commit-chain diff-id (1+ n)))))))
   (build-commit-chain (getf latest-diff :id)))))

(defun build-raw-commit (revision)
 (let
  ((raw-diff
    (drakma:http-request
     (format nil
      "~A/D~A?download=true"
      *phabricator-location*
      (getf revision :id))))
   (parent-commit-sha
    (cl-ppcre:regex-replace-all
     "\\s"
     (with-output-to-string (out)
      (sb-ext:run-program "/usr/bin/git"
       (list
        (format nil "--git-dir=~A" (getf (getf revision :repository) :localpath))
        "rev-list"
        "-1"
        (multiple-value-bind
         (s m h day mon year) (decode-universal-time (unix-to-universal-time (getf revision :datecreated)))
         (declare (ignore s m h))
         (format nil "--before='~A/~A/~A'" mon day year))
        ; Default to master branch, but may need to change later
        "master")
       :output out))
     "")))
  (list
   :repositoryid (getf (getf revision :repository) :id)
   :raw-diff raw-diff
   :parents
   (list
    (list
     :repositoryid (getf (getf revision :repository) :id)
     :commitidentifier parent-commit-sha)))))

(defun get-revisions ()
 (mapcar
  (lambda (rev)
   (let
    ((repository (get-repository (getf rev :repositoryphid))))
    (append
     rev
     (list :related-commits
      (or
       (get-commits-from-db rev)
       (ignore-errors (get-commits-from-staging rev))
       (build-raw-commit rev)))
     (list :repository repository))))
  (remove-if
   (lambda (rev) (find (getf rev :id) *revisions-to-skip*))
   (query "select id, title, summary, phid, status, repositoryphid, datecreated from phabricator_differential.differential_revision"))))

(defun convert-commit-to-core (commit)
 (cond
  ((getf commit :commitidentifier)
   (forgerie-core:make-commit :sha (getf commit :commitidentifier)))
  ((getf commit :raw-diff)
   (forgerie-core:make-patch :diff (getf commit :raw-diff)))
  ((getf commit :patch)
   (forgerie-core:make-patch :diff (getf commit :patch)))))

(defun convert-revision-to-core (revision-def)
 (let
  ((type
    (cond
     ((find (getf revision-def :status) (list "published" "abandoned") :test #'string=)
      :closed)
     ((find (getf revision-def :status) (list "changes-planned" "needs-review" "needs-revision") :test #'string=)
      :open)
     (t (error "Unknown revision type: ~A" (getf revision-def :status))))))

  (forgerie-core:make-merge-request
   :title (getf revision-def :title)
   :description (map 'string #'code-char (getf revision-def :summary))
   :vc-repository (convert-repository-to-core (getf revision-def :repository))
   :type type
   :target-branch
   (forgerie-core:make-branch
    :name
    ; Defaults to master, but that may be wrong after more investigation
    (if (eql :open type) "master" (format nil "generated-differential-D~A-target" (getf revision-def :id)))
    :commit (convert-commit-to-core (car (last (getf revision-def :related-commits)))))
   :source-branch
   (forgerie-core:make-branch
    :name (format nil "generated-differential-D~A-source" (getf revision-def :id))
    ; We don't need
    :commit (convert-commit-to-core (car (last (getf revision-def :related-commits)))))
   :changes (mapcar #'convert-commit-to-core (getf revision-def :related-commits)))))

(defun convert-repository-to-core (repository-def)
 (forgerie-core:make-vc-repository
  :name (getf repository-def :name)
  :slug (getf repository-def :repositoryslug)
  :projects (mapcar #'convert-project-to-core (getf repository-def :projects))
  :primary-projects (mapcar #'convert-project-to-core (getf repository-def :primary-projects))
  :git-location
  (format nil "~A~A"
   *git-location*
   (car (last (pathname-directory (pathname (getf repository-def :localpath))))))))

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
