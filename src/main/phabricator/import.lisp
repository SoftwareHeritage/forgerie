(in-package #:forgerie-phabricator)

; This is really a stepping stone to more structured data, but nice
; while what we're getting out of the database and whatnot is more fluid.
(defmacro getf-convenience (type &rest fields)
`(progn
,@(mapcar
   (lambda (field)
   `(defun ,(intern (format nil "~A-~A" type field)) (o)
     (getf o ,(intern (symbol-name field) :keyword))))
   fields)))

(getf-convenience differential-diff id)
(getf-convenience edge dst)
(getf-convenience email address isprimary)
(getf-convenience file storageengine storageformat storagehandle name data)
(getf-convenience file-storageblob data)
(getf-convenience paste id title filephid file)
(getf-convenience project phid icon name)
(getf-convenience project-slug slug)
(getf-convenience repository id phid repositoryslug name localpath projects primary-projects)
(getf-convenience repository-commit id phid repositoryid commitidentifier parents patch)
(getf-convenience task id phid title projects)
(getf-convenience user username realname phid emails)
(getf-convenience differential-revision id title summary phid status repository repositoryphid datecreated related-commits authorphid)

(defvar *query-cache* nil)

(defun query (query)
 (when (not (assoc query *query-cache* :test #'string=))
  (setf *query-cache*
   (cons
    (cons
     query
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
    *query-cache*)))
 (cdr (assoc query *query-cache* :test #'string=)))

(defun initialize ()
 (cl-mysql:connect :password *database-password*)
 (cl-mysql:query "set names 'utf8'"))

; This function is only for development mode.  While we have emails
; turned off for gitlab, there's a chance that something screwed up will happen
; so we should make it so the aren't real email addresses
(defun sanitize-address (address)
 (format nil "~A@opentechstrategies.com" (cl-ppcre:regex-replace-all "@" address "_")))

(defun user-primary-email (user)
 (find 1 (user-emails user) :key #'email-isprimary))

(defun get-emails (user-phid)
 (query (format nil "select * from phabricator_user.user_email where userphid = '~A'" user-phid)))

(defun attach-emails-to-user (user)
 (append
  user
  (list :emails (get-emails (user-phid user)))))

(defun get-user (phid)
 (attach-emails-to-user
  (first
   (query (format nil "select username, realName, phid from phabricator_user.user where phid = '~A'" phid)))))

(defun get-users ()
 (mapcar #'attach-emails-to-user
  (query "select username, realName, phid from phabricator_user.user")))

(defun fill-out-project (proj)
 (append
  (list
   :tags
   (mapcar #'project-slug-slug
    (query (format nil "select slug from phabricator_project.project_slug where projectphid = '~A'" (project-phid proj)))))
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
     (mapcar #'get-project
      (mapcar #'edge-dst
       (query
        (format nil
         "select dst from phabricator_maniphest.edge where src = '~A' and dst like 'PHID-PROJ%'"
         (task-phid task))))))))
  (query "select * from phabricator_maniphest.maniphest_task")))

(defun attach-projects-to-repository (repo)
 (let
  ((associated-projects
    (mapcar #'get-project
     (mapcar #'edge-dst
      (query
       (format nil
        "select * from phabricator_repository.edge where src = '~A' and dst like 'PHID-PROJ%'"
        (repository-phid repo)))))))
  (append
   repo
   (list :primary-projects (remove-if-not (lambda (project) (string= "folder" (project-icon project))) associated-projects))
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
       (string= "blob" (file-storageengine file))
       (string= "raw" (file-storageformat file)))
      (map 'string #'code-char
       (file-storageblob-data
        (first
         (query
          (format nil "select data from phabricator_file.file_storageblob where id = '~A';"
           (file-storagehandle file)))))))
     (t
      (error
       "Don't know how to handle files of with engine/format of ~A/~A encounted on ~A"
       (file-storageengine file)
       (file-storageformat file)
       file-phid)))))))

(defun get-pastes ()
 (remove
  nil
  (mapcar
   (lambda (paste)
    (let
     ; ignore-errors here is due to the nature of the data we're working with,
     ; and should probably get removed later on
     ((file (ignore-errors (get-file (paste-filephid paste)))))
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
      (mapcar #'repository-commit-phid
       (query
        (format nil
         "select rc.phid
             from phabricator_repository.repository_parents rp
             join phabricator_repository.repository_commit rc on rp.parentcommitid = rc.id
             where childcommitid = '~A'"
         (repository-commit-id commit)))))
     :unfetched)))))

(defun order-related-commits (commits)
 (when (find-if (lambda (commit) (< 1 (length (repository-commit-parents commit)))) commits)
  (error "There's a merge commit in the differential commit list?!  Investigate further"))
 (cond
  ((not commits) nil)
  ((= 1 (length commits)) commits)
  (t
   (let*
    ((parents (apply #'append (mapcar #'repository-commit-parents commits)))
     (non-parent-commits
      (remove-if
       (lambda (commit)
        (find (repository-commit-commitidentifier commit) parents :key #'repository-commit-commitidentifier :test #'string=))
       commits)))
    (when (< 1 (length non-parent-commits))
     (format t "~S~%" non-parent-commits)
     (error "There's multiple commits that are not a parent in the set, meaning this commit chain is weird"))
    (cons
     (car non-parent-commits)
     (order-related-commits (remove (car non-parent-commits) commits)))))))

(defun get-commits-from-db (revision)
 (let
  ((repository (get-repository (differential-revision-repositoryphid revision))))
  (reverse
   (order-related-commits
    (remove-if
     (lambda (commit)
      (or
       (not (eql (repository-commit-repositoryid commit) (repository-id repository)))
       ; Is this commit reachable?
       (not
        (zerop
         (forgerie-core:git-cmd
          (repository-localpath repository)
          "cat-file"
          (list "-t" (repository-commit-commitidentifier commit)))))
       (string=
        (format nil "undefined~%")
        (second
         (multiple-value-list
          (forgerie-core:git-cmd
           (repository-localpath repository)
           "name-rev"
           (list
            "--name-only"
            (repository-commit-commitidentifier commit))))))
       ; Remove merge commits
       (< 1 (length (repository-commit-parents commit)))))
     (mapcar #'get-commit
      (mapcar #'edge-dst
       ; type of 31 is the same as DifferentialRevisionHasCommitEdgeType
       (query (format nil "select dst from phabricator_differential.edge where src = '~A' and type = 31"
               (differential-revision-phid revision))))))))))

(defvar *sha-detail-cache*
 (when (probe-file "~/shadetailcache") (with-open-file (str "~/shadetailcache" :direction :input) (read str nil))))

(defun save-details ()
 (with-open-file (str "~/shadetailcache" :direction :output :if-exists :supersede)
  (format str "~S" *sha-detail-cache*)))


(defun get-details (repository sha)
 (with-output-to-string (out)
  (sb-ext:run-program (asdf:system-relative-pathname :forgerie "bin/getdetails.sh")
  (list sha (repository-localpath repository))
  :wait t
  :output out)))

(defun get-shas-and-details (repository)
 (when
  (not (assoc (repository-phid repository) *sha-detail-cache* :test #'string=))
  (setf
   *sha-detail-cache*
   (cons
    (cons
     (repository-phid repository)
     (mapcar
      (lambda (sha) (list sha (get-details repository sha)))
      (cl-ppcre:split
       "\\n"
       (second
        (multiple-value-list
         (forgerie-core:git-cmd
          (repository-localpath repository)
          "log"
          (list
           "--all"
           "--pretty=%H")))))))
    *sha-detail-cache*)))
 (cdr (assoc (repository-phid repository) *sha-detail-cache* :test #'string=)))

(defun get-commits-from-staging (revision)
 (let*
  ((staging-repository (get-repository "PHID-REPO-cuxcaqw5u7vepi4b4bpg"))
   (repository (get-repository (differential-revision-repositoryphid revision)))
   (latest-diff
    (first
     (query
      (format nil "select id from phabricator_differential.differential_diff where revisionid = '~A' order by id desc limit 1"
       (differential-revision-id revision)))))
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
       (list (list :commitidentifier (car repo-details) :repository repository))
       (cons
        (list
         :patch
         (second
          (multiple-value-list
           (forgerie-core:git-cmd
            (repository-localpath staging-repository)
            "format-patch"
            (list "-k" "-1" "--stdout" (format nil "phabricator/diff/~A~~~A" diff-id n))))))
       (build-commit-chain diff-id (1+ n)))))))
   (let
    ((commit-chain (reverse (build-commit-chain (differential-diff-id latest-diff)))))
    (cons
     (append
      (second commit-chain)
      (list :parents (list (first commit-chain))))
     (cddr commit-chain))))))

(defun build-raw-commit (revision)
 (let*
  ((repository (get-repository (differential-revision-repositoryphid revision)))
   (user (get-user (differential-revision-authorphid revision)))
   (path (format nil "~A/~A/" *checkout-path* (repository-repositoryslug repository)))
   (raw-diff
    (drakma:http-request
     (format nil
      "~A/D~A?download=true"
      *phabricator-location*
      (differential-revision-id revision)))))
   (ensure-directories-exist path)
   (forgerie-core:git-cmd path "clone" (list (repository-localpath repository) "."))
   (labels
    ((sha-applicable (sha)
      (forgerie-core:git-cmd path "checkout" (list sha))
      (zerop
       (sb-ext:process-exit-code
        (with-input-from-string (in raw-diff)
         (forgerie-core:git-cmd path "apply" (list "-") :input in)))))
     (find-parent-sha (&optional (shas (mapcar #'car (get-shas-and-details repository))))
      (with-output-to-string (out)
       (cond
        ((not shas) (error "Couldn't find a sha for which this could be applied"))
        ((sha-applicable (car shas)) (car shas))
        (t (find-parent-sha (cdr shas)))))))
    (let
     ((parent-commit-sha (find-parent-sha)))
     (forgerie-core:git-cmd path "add" (list "."))
     (forgerie-core:git-cmd path "commit"
      (list
       "--author"
       (format nil "~A <~A>" (user-realname user) (email-address (user-primary-email user)))
       "-m"
       (format nil "Generated commit for differential D~A" (differential-revision-id revision))))
     (list
      (list
       :repositoryid (repository-id repository)
       :patch
       (second
        (multiple-value-list
        (forgerie-core:git-cmd path "format-patch"
         (list
          "-k"
          "-1"
          "--stdout"))))
       :parents
       (list
        (list
         :repositoryid (repository-id repository)
         :commitidentifier parent-commit-sha))))))))

(defun get-revisions ()
 (mapcar
  (lambda (rev)
   (let
    ((repository (get-repository (differential-revision-repositoryphid rev))))
    (append
     rev
     (list :related-commits
      (or
       (get-commits-from-db rev)
       (ignore-errors (get-commits-from-staging rev))
       (build-raw-commit rev)))
     (list :repository repository))))
  (remove-if
   (lambda (rev) (find (differential-revision-id rev) *revisions-to-skip*))
   ; 4700 here is just for testing purposes, so that we limit to only 300 or so diffs
   (query "select id, title, summary, phid, status, repositoryphid, datecreated, authorphid from phabricator_differential.differential_revision where id > 4700"))))

(defun convert-commit-to-core (commit)
 (cond
  ((repository-commit-commitidentifier commit)
   (forgerie-core:make-commit :sha (repository-commit-commitidentifier commit)))
  ((repository-commit-patch commit)
   (forgerie-core:make-patch :diff (repository-commit-patch commit)))))

(defun convert-revision-to-core (revision-def)
 (let
  ((type
    (cond
     ((find (differential-revision-status revision-def) (list "published" "abandoned") :test #'string=)
      :closed)
     ((find (differential-revision-status revision-def) (list "changes-planned" "needs-review" "needs-revision" "accepted") :test #'string=)
      :open)
     (t (error "Unknown revision type: ~A" (differential-revision-status revision-def))))))

  (forgerie-core:make-merge-request
   :title (differential-revision-title revision-def)
   :description (map 'string #'code-char (differential-revision-summary revision-def))
   :vc-repository (convert-repository-to-core (differential-revision-repository revision-def))
   :type type
   :target-branch
   (forgerie-core:make-branch
    :name
    ; Defaults to master, but that may be wrong after more investigation
    (if (eql :open type) "master" (format nil "generated-differential-D~A-target" (differential-revision-id revision-def)))
    :commit (convert-commit-to-core (car (repository-commit-parents (car (differential-revision-related-commits revision-def))))))
   :source-branch
   (forgerie-core:make-branch
    :name (format nil "generated-differential-D~A-source" (differential-revision-id revision-def))
    :commit (convert-commit-to-core (car (repository-commit-parents (car (differential-revision-related-commits revision-def))))))
   :changes (mapcar #'convert-commit-to-core (differential-revision-related-commits revision-def)))))

(defun convert-repository-to-core (repository-def)
 (forgerie-core:make-vc-repository
  :name (repository-name repository-def)
  :slug (repository-repositoryslug repository-def)
  :projects (mapcar #'convert-project-to-core (repository-projects repository-def))
  :primary-projects (mapcar #'convert-project-to-core (repository-primary-projects repository-def))
  :git-location
  (format nil "~A~A.git"
   *git-location*
   (repository-repositoryslug repository-def))))

(defun convert-project-to-core (project-def)
 (forgerie-core:make-project
  :name (project-name project-def)))

(defun convert-email-to-core (email-def)
 (forgerie-core:make-email
  :address (sanitize-address (email-address email-def))
  :is-primary (eql (email-isprimary email-def) 1)))

(defun convert-user-to-core (user-def)
 (forgerie-core:make-user
  :username (user-username user-def)
  :name (user-realname user-def)
  :emails (mapcar #'convert-email-to-core (user-emails user-def))))

(defun convert-task-to-core (task-def)
 (forgerie-core:make-ticket
  :id (task-id task-def)
  :title (task-title task-def)
  :projects (mapcar #'convert-project-to-core (task-projects task-def))))

(defun convert-file-to-core (file-def)
 (forgerie-core:make-file
  :name (file-name file-def)
  :data (file-data file-def)))

(defun convert-paste-to-core (paste-def)
 (forgerie-core:make-snippet
  :id (paste-id paste-def)
  :title (paste-title paste-def)
  :files (list (convert-file-to-core (paste-file paste-def)))))

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
  :merge-requests
  (mapcar #'convert-revision-to-core (get-revisions))
  :tickets
  (mapcar #'convert-task-to-core (get-tasks))))
