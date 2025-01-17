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
(getf-convenience file id storageengine storageformat storagehandle name location mimetype bytesize phid)
(getf-convenience file-storageblob data)
(getf-convenience paste id phid title filephid file comments author authorphid datecreated)
(getf-convenience paste-comment id author authorphid content datecreated)
(getf-convenience project id phid icon name tags)
(getf-convenience project-slug slug)
(getf-convenience repository id phid repositoryslug name localpath projects primary-projects commits spacephid)
(getf-convenience repository-commit id phid repositoryid commitidentifier parents patch comments git-comment)
(getf-convenience task id phid title status projects comments owner author ownerphid authorphid description datecreated priority spacephid linked-tasks subscribers)
(getf-convenience task-comment id author authorphid content datecreated)
(getf-convenience user id username realname phid emails isadmin profileimage profileimagephid)
(getf-convenience differential-revision
 id title summary testplan phid status repository repositoryphid datecreated related-commits author authorphid comments change-comments
 activediffphid)
(getf-convenience differential-transaction-comment
 phid content changesetid isnewfile linenumber linelength replytocommentphid diff replies author authorphid datecreated)
(getf-convenience differential-diff sourcecontrolbaserevision filename phid)
(getf-convenience differential-comment id author authorphid content datecreated)

(defvar *query-cache* nil)

(defun query (query)
 (when (not (assoc query *query-cache* :test #'string=))
  (when forgerie-core:*debug* (format t "~S~%" query))
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

; https://github.com/hackinghat/cl-mysql/blob/3fbf6e1421484f64c5bcf2ff3c4b96c6f0414f09/pool.lisp#L283
(defun initialize ()
  (if *database-host*
    (cl-mysql:connect :host *database-host* :port *database-port*
                      :user *database-username* :password *database-password* )
  (cl-mysql:connect :user *database-username* :password *database-password*))
 (cl-mysql:query "set names 'utf8'"))

(defun sanitize-address (address)
 (when *email-address-sanitizer*
  (funcall *email-address-sanitizer* address)))

(defun user-primary-email (user)
 (find 1 (user-emails user) :key #'email-isprimary))

(defun get-emails (user-phid)
 (query (format nil "select * from phabricator_user.user_email where userphid = '~A'" user-phid)))

(defun annotate-user (user)
 (append
  (let
   ((override (find (user-id user) *user-overrides* :key (lambda (override) (getf override :key)))))
   (when (and override (eql :update (getf override :action)))
    (getf override :data)))
  user
  (list :profileimage (when (user-profileimagephid user) (get-file (user-profileimagephid user))))
  (list :emails (get-emails (user-phid user)))))

(defun get-user (phid)
 (annotate-user
  (first
   (query (format nil "select id, username, realName, phid, isadmin, profileimagephid from phabricator_user.user where phid = '~A'" phid)))))

(defun get-users ()
 (mapcar #'annotate-user
  (query "select id, username, realName, phid, isadmin, profileimagephid from phabricator_user.user")))

(defun fill-out-project (proj)
 (append
  (list
   :tags
   (mapcar #'project-slug-slug
    (query (format nil "select slug from phabricator_project.project_slug where projectphid = '~A'" (project-phid proj)))))
  proj))

(defun get-project (id &optional (key "phid"))
 (fill-out-project
  (first
   (query
    (format nil "select id, phid, color, name, icon from phabricator_project.project where ~A = '~A'" key id)))))

(defun get-projects ()
 (mapcar #'fill-out-project (query "select id, phid, color, name, icon from phabricator_project.project")))

(defun add-author-to-task-comment (comment)
 (append
  comment
  (list :author (get-user (task-comment-authorphid comment)))))

(defun get-task-comments (task)
 (mapcar
  #'add-author-to-task-comment
  (query
   (format nil
    "select
        mtc.id, mtc.authorphid, mt.datecreated, mtc.content
        from phabricator_maniphest.maniphest_transaction mt
        left join phabricator_maniphest.maniphest_transaction_comment mtc on mtc.phid = mt.commentphid
        where commentphid is not null and
            mtc.isdeleted = 0 and
            objectphid = '~A' and
            transactiontype = 'core:comment' order by mt.datecreated"
    (task-phid task)))))

(defun annotate-task (task)
 (append
  task
  (list
   :owner (when (task-ownerphid task) (get-user (task-ownerphid task)))
   :author (when (task-authorphid task) (get-user (task-authorphid task)))
   :comments (get-task-comments task))
  (list
   :subscribers
   (mapcar
    (lambda (phid) (get-user phid))
     (mapcar #'edge-dst
      (query
       (format nil
        "select dst from phabricator_maniphest.edge where src = '~A' and type = 21"
        (task-phid task))))))
  (list
   :linked-tasks
   (mapcar
    (lambda (phid) (get-task phid :shallow t))
    (mapcar #'edge-dst
     (query
      (format nil
       "select dst from phabricator_maniphest.edge where src = '~A' and type = 3"
       (task-phid task))))))
  (list
   :projects
   (mapcar #'get-project
    (mapcar #'edge-dst
     (query
      (format nil
       "select dst from phabricator_maniphest.edge where src = '~A' and dst like 'PHID-PROJ%'"
       (task-phid task))))))))

(defun get-task (phid &key shallow)
 (let
  ((task (first (query (format nil "select * from phabricator_maniphest.maniphest_task where phid = '~A'" phid)))))
  (if shallow task (annotate-task task))))

(defun get-tasks ()
 (mapcar #'annotate-task
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
   (let
    ((override (find (repository-id repo) *repository-overrides* :key (lambda (override) (getf override :key)))))
    (when (and override (eql :update (getf override :action)))
     (getf override :data)))
   repo
   (list :primary-projects
    (append
     (mapcar
      (lambda (override) (get-project (getf override :key) "id"))
      (remove-if-not
       (lambda (override)
        (and
         (repository-repositoryslug repo)
         (string= (repository-repositoryslug repo) (getf override :repository))))
       *project-assignment-overrides*))
     (remove
      nil
      (mapcar
       (lambda (project)
        (when
         (and
          (string= "folder" (project-icon project))
          ; We remove projects that have override defs, because we add them back in later
          (not (find (project-id project) *project-assignment-overrides* :key (lambda (override) (getf override :key)))))
         project))
       associated-projects))))
   (list :projects associated-projects))))

(defun annotate-repository-commits (repo)
 (append
  (list
   :commits
   (cached "repository-commits" (repository-phid repo)
    (mapcar
     (lambda (sha)
      (list
       :commitidentifier sha
       :git-comment
       (nth-value 1
        (forgerie-core:git-cmd
         (repository-localpath repo)
         "log"
         (list "--format=%B" "-n" "1" sha)))))
     (mapcar #'car (get-shas-and-details repo)))))
  repo))

(defun get-repository (phid)
 (attach-projects-to-repository
  (first
   (query
    (format nil
     "select id, phid, repositoryslug, name, localpath, spacephid from phabricator_repository.repository where phid = '~A'"
     phid)))))

(defun get-repository-by-slug (slug)
 (attach-projects-to-repository
  (first
   (query
    (format nil
     "select id, phid, repositoryslug, name, localpath, spacephid from phabricator_repository.repository where repositoryslug = '~A'"
     slug)))))

(defun get-repository-by-id (id)
 (attach-projects-to-repository
  (first
   (query
    (format nil
     "select id, phid, repositoryslug, name, localpath, spacephid from phabricator_repository.repository where id = '~A'"
     id)))))

(defun get-repositories ()
 (let
  ((repositories
    (remove-if
     (lambda (repository)
      (and
       *included-repositories*
       (not (find (repository-repositoryslug repository) *included-repositories* :test #'string=))))
     (query "select id, phid, repositoryslug, name, localpath, spacephid from phabricator_repository.repository where repositoryslug is not null"))))
  (mapcar #'annotate-repository-commits
   (mapcar #'attach-projects-to-repository
    (remove-if
     (lambda (repo)
      (eql :skip
       (getf
        (find (repository-id repo) *repository-overrides* :key (lambda (override) (getf override :key)))
         :action)))
     repositories)))))

(defun db-file (file-phid)
 (first
  (query
   (format nil "select id, phid, name, storageEngine, storageFormat, storageHandle, mimetype, bytesize from phabricator_file.file where phid = '~A'"
    file-phid))))

(defun put-file-on-disk (out file)
 (cond
  ((and (string= "blob" (file-storageengine file)) (string= "raw" (file-storageformat file)))
   (write-sequence
    (file-storageblob-data
     (first
      (query
       (format nil "select data from phabricator_file.file_storageblob where id = '~A';"
        (file-storagehandle file)))))
    out))
  ((and
    (string= "local-disk" (file-storageengine file))
    (string= "raw" (file-storageformat file)))
   (with-open-file (str (format nil "~A/~A" *storage-location* (file-storagehandle file)) :element-type 'unsigned-byte)
    (let
     ((data (make-array (file-bytesize file))))
     (read-sequence data str)
     (write-sequence data out))))
  ((string= "chunks" (file-storageengine file))
   (mapcar
    (lambda (chunk)
     (put-file-on-disk out (db-file (getf chunk :datafilephid)))
     (force-output out))
    (query
     (format nil "select dataFilePHID from phabricator_file.file_chunk where chunkhandle = '~A' order by byteStart" (file-storagehandle file)))))
  (t
   (error
    "Don't know how to handle files of with engine,format,mimetype of ~A,~A,~A encounted on ~A"
    (file-storageengine file)
    (file-storageformat file)
    (file-mimetype file)
    (file-phid file)))))

(defun get-file (file-phid)
 (let*
  ((file (db-file file-phid))
   (dir (format nil "~A/files/~A/" *working-directory* (subseq file-phid (- (length file-phid) 3))))
   (location (format nil "~A~A" dir file-phid)))
  (when (not (probe-file location))
   (ensure-directories-exist dir)
   (with-open-file (out location :direction :output :element-type 'unsigned-byte)
    (put-file-on-disk out file)))
  (append file (list :location location))))

(defun get-captured-files ()
 (mapcar
  #'get-file
  (mapcar
   (lambda (file-id)
    (getf (first (query (format nil "select phid from phabricator_file.file where id = ~A" file-id))) :phid))
   (with-open-file (str (format nil "~A/everything/captured-files" *working-directory*))
    (remove-duplicates
     (loop :for obj := (read str nil)
      :while obj
      :collect obj)
     :test #'string=)))))

(defun capture-file (id)
 (with-open-file (str
                  (format nil "~A/everything/captured-files" *working-directory*)
                  :direction :output
                  :if-exists :append
                  :if-does-not-exist :create)
  (format str "~S" id)))

(defun add-author-to-paste-comment (comment)
 (append
  comment
  (list :author (get-user (paste-comment-authorphid comment)))))

(defun get-paste-comments (paste)
 (mapcar
  #'add-author-to-paste-comment
  (query
   (format nil
    "select
        ptc.id, ptc.authorphid, pt.datecreated, ptc.content
        from phabricator_paste.paste_transaction pt
        left join phabricator_paste.paste_transaction_comment ptc on ptc.phid = pt.commentphid
        where commentphid is not null and
            ptc.isdeleted = 0 and
            objectphid = '~A' and
            transactiontype = 'core:comment' order by pt.datecreated"
    (paste-phid paste)))))

(defun get-pastes ()
 (mapcar
  (lambda (paste)
   (append paste
    (list
     :author (get-user (paste-authorphid paste))
     :comments (get-paste-comments paste))))
  (remove
   nil
   (mapcar
    (lambda (paste)
     (let
      ((file (get-file (paste-filephid paste))))
      (when file (append (list :file file) paste))))
    (remove-if
     (lambda (paste) (find (paste-id paste) *pastes-to-skip*))
     (query "select id, title, phid, filePHID, datecreated, authorPHID from phabricator_paste.paste"))))))

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
        (nth-value 1
         (forgerie-core:git-cmd
          (repository-localpath repository)
          "name-rev"
          (list
           "--name-only"
           (repository-commit-commitidentifier commit)))))
       ; Remove merge commits
       (< 1 (length (repository-commit-parents commit)))))
     (mapcar #'get-commit
      (mapcar #'edge-dst
       ; type of 31 is the same as DifferentialRevisionHasCommitEdgeType
       (query (format nil "select dst from phabricator_differential.edge where src = '~A' and type = 31"
               (differential-revision-phid revision))))))))))

(defun get-details (repository sha)
 (with-output-to-string (out)
  (sb-ext:run-program (asdf:system-relative-pathname :forgerie "bin/getdetails.sh")
  (list sha (repository-localpath repository))
  :wait t
  :output out)))

(defun get-shas-and-details (repository)
 (forgerie-core:check-for-stop)
 (cached
  "shas-and-details"
  (repository-phid repository)
  (mapcar
   (lambda (sha) (list sha (get-details repository sha)))
   (cl-ppcre:split
    "\\n"
    (nth-value 1
     (forgerie-core:git-cmd
      (repository-localpath repository)
      "log"
      (list
       "--all"
       "--pretty=%H")))))))

(defun get-commits-from-staging (revision)
 (let*
  ((staging-repository (get-repository *staging-repository*))
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
         (nth-value 1
          (forgerie-core:git-cmd
           (repository-localpath staging-repository)
           "format-patch"
           (list "-k" "-1" "--stdout" (format nil "phabricator/diff/~A~~~A" diff-id n)))))
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
   (path (format nil "~A/~A/" *working-directory* (repository-repositoryslug repository)))
   (raw-diff
    (dex:get
     (format nil
      "~A/D~A?download=true"
      *phabricator-location*
      (differential-revision-id revision)))))
  (when (not (probe-file path))
   (ensure-directories-exist path)
   (forgerie-core:git-cmd path "clone" (list (repository-localpath repository) ".")))
  (labels
   ((sha-applicable (sha)
     (forgerie-core:git-cmd path "checkout" (list sha))
     (zerop
      (with-input-from-string (in raw-diff)
       (forgerie-core:git-cmd path "apply" (list "-") :input in :error nil))))
    (find-parent-sha (&optional (shas (mapcar #'car (get-shas-and-details repository))))
     (cond
      ((not shas)
       (with-open-file (debug-file "~/diff.patch" :direction :output :if-exists :supersede)
        (princ raw-diff debug-file))
       (error "Couldn't find a sha for which this could be applied"))
      ((sha-applicable (car shas)) (car shas))
      (t (find-parent-sha (cdr shas))))))
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
      (nth-value 1
       (forgerie-core:git-cmd path "format-patch"
        (list
         "-k"
         "-1"
         "--stdout")))
      :parents
      (list
       (list
        :repositoryid (repository-id repository)
        :commitidentifier parent-commit-sha))))))))

(defun add-author-to-differential-comment (comment)
 (append
  comment
  (list :author (get-user (differential-comment-authorphid comment)))))

(defun get-revision-comments (rev)
 (mapcar
  #'add-author-to-differential-comment
  (query
   (format nil
    "select
        rtc.id, rtc.authorphid, rt.datecreated, rtc.content
        from phabricator_differential.differential_transaction rt
        left join phabricator_differential.differential_transaction_comment rtc on rtc.phid = rt.commentphid
        where commentphid is not null and
            rtc.isdeleted = 0 and
            objectphid = '~A' and
            transactiontype = 'core:comment' order by rt.datecreated"
    (differential-revision-phid rev)))))

(defun get-revision-inline-comments (rev)
 (let*
  ((phid (differential-revision-phid rev))
   (comments
    (query
     (format nil
      "select * from phabricator_differential.differential_transaction_comment where revisionphid = '~A' and isdeleted = 0 and changesetid is not null" phid))))
  (mapcar
   (lambda (comment)
    (append
     comment
     (list
      :author
      (get-user (differential-transaction-comment-authorphid comment))
      :diff
      (car
       (query
        (format nil
         "select diff.*, changeset.filename from phabricator_differential.differential_diff diff join phabricator_differential.differential_changeset changeset on changeset.diffid = diff.id where changeset.id = ~A" (differential-transaction-comment-changesetid comment)))))))
  comments)))

(defun attach-inline-comments-to-commits (commits inline-comments)
 (flet
  ((comment-attached-to-commit (comment commit)
    (find
     (differential-diff-sourcecontrolbaserevision
      (differential-transaction-comment-diff comment))
     (mapcar #'repository-commit-commitidentifier (repository-commit-parents commit))
     :test #'string=)))
  (let
   ((attached-comments
     (remove-if
      (lambda (comment)
       (notany
        (lambda (commit) (comment-attached-to-commit comment commit))
        commits))
      inline-comments))
    (unattached-comments
     (remove-if-not
      (lambda (comment)
       (notany
        (lambda (commit) (comment-attached-to-commit comment commit))
        commits))
      inline-comments)))
   (values
    (mapcar
     (lambda (commit)
      (setf
       (getf commit :comments)
       (remove-if-not
        (lambda (comment)
         (comment-attached-to-commit comment commit))
        attached-comments))
      commit)
     commits)
    unattached-comments))))

(defun thread-inline-comments (comments)
 (labels
  ((thread-comment (comment-to-thread comments)
    (when comments
     (mapcar
      (lambda (comment)
       (if
        (string=
         (differential-transaction-comment-replytocommentphid comment-to-thread)
         (differential-transaction-comment-phid comment))
        (progn
         (setf
          (getf comment :replies)
          (append
           (differential-transaction-comment-replies comment)
           (list comment-to-thread)))
          comment)
        (progn
         (setf
          (getf comment :replies)
          (thread-comment comment-to-thread (differential-transaction-comment-replies comment)))
         comment)))
     comments))))
  (let
   ((comment-to-thread (find-if #'differential-transaction-comment-replytocommentphid comments)))
   (if
    (not comment-to-thread)
    comments
    (thread-inline-comments
     (thread-comment
      comment-to-thread
      (remove comment-to-thread comments :test #'equalp)))))))

(defun get-revision-commits (rev)
 (let
  ((inline-comments (thread-inline-comments (get-revision-inline-comments rev)))
   (commits
    (cached
    "revision_commits"
    (differential-revision-id rev)
    (or
     (get-commits-from-db rev)
     (when *staging-repository*
      (handler-case
       (get-commits-from-staging rev)
       (error (e) (format t "Failed to get commit from staging due to error ~A, falling back.~%" e))))
     (build-raw-commit rev)))))
  (attach-inline-comments-to-commits
   commits
   inline-comments)))

(defun annotate-revision (rev)
 (forgerie-core:check-for-stop)
 (when forgerie-core:*debug*
  (format t "---------------~%Loading revision ~A~%~%~%" (differential-revision-id rev)))
 (let
  ((repository (get-repository (differential-revision-repositoryphid rev))))
  (when
   (or
    (not *included-repositories*)
    (find (repository-repositoryslug repository) *included-repositories* :test #'string=))
   (handler-case
    (cached
     "revisions"
     (differential-revision-id rev)
     (append
      rev
      (list :author (get-user (differential-revision-authorphid rev)))
      (list :comments (get-revision-comments rev))
      (multiple-value-bind (commits unattached-comments) (get-revision-commits rev)
       (let
        ((comments-to-attach
          (remove-if-not
           (lambda (comment)
            (string= (differential-diff-phid (differential-transaction-comment-diff comment)) (differential-revision-activediffphid rev)))
           unattached-comments)))
        (list :change-comments comments-to-attach :related-commits commits)))
      (list :repository repository)))
    (error (e) (format t "Failed to handle revision ~A, due to error ~A, skipping.~%" (differential-revision-id rev) e))))))

(defun get-revision (id)
 (car
  (query (format nil "select id, title, summary, testplan, phid, status, repositoryphid, datecreated, authorphid, activediffphid from phabricator_differential.differential_revision where id = ~A" id))))

(defun get-revisions ()
 (remove
  nil
  (mapcar #'annotate-revision
   (remove-if
    (lambda (rev) (find (differential-revision-id rev) *revisions-to-skip*))
    (query "select id, title, summary, testplan, phid, status, repositoryphid, datecreated, authorphid, activediffphid from phabricator_differential.differential_revision")))))

(defun parse-comment (comment)
 (let
  ; This is an oddity in how phabricator represents this part of markdown, and thus it's converted
  ; to actual markdown (checkbox list items need to be prefaced by a list element like -)
  ((comment (cl-ppcre:regex-replace-all "\\n( *)\\[(.)\\]" comment (format nil "~%\\1 - [\\2]"))))
  (labels
   ((first-instance-of (regex type &key with-aftercheck (comment comment))
     (multiple-value-bind (start end match-starts match-ends) (cl-ppcre:scan regex comment)
      (cond
       ((not start) nil)
       ((eql type :link)
        (list start end type
         (list
          (subseq comment (aref match-starts 0) (aref match-ends 0))
          (subseq comment (aref match-starts 1) (aref match-ends 1)))
         (subseq comment start end)))
       ((or (zerop start) (= end (length comment)))
        (list start end type (subseq comment (aref match-starts 0) (aref match-ends 0)) (subseq comment start end)))
       ((and with-aftercheck (cl-ppcre:scan "[\\d\\w]" (subseq comment (1- start) start)))
        (first-instance-of regex type :comment (subseq comment end)))
       ((and with-aftercheck (cl-ppcre:scan "[\\d\\w]" (subseq comment end (1+ end))))
        (first-instance-of regex type :comment (subseq comment end)))
       (t
        (list start end type (subseq comment (aref match-starts 0) (aref match-ends 0)) (subseq comment start end)))))))
   (let*
    ((first-instance
      (car
       (sort
        (remove-if-not #'identity
         (list
          (first-instance-of "\\n= ([^\\n]*) =\\n" :h1)
          (first-instance-of "\\n== ([^\\n]*) ==\\n" :h2)
          (first-instance-of "\\n=== ([^\\n]*) ===\\n" :h3)
          (first-instance-of "\\n==== ([^\\n]*) ====\\n" :h4)
          (first-instance-of "\\n===== ([^\\n]*) =====\\n" :h5)
          (first-instance-of "\\[\\[ *([^| ]*) *\\| *([^\\]]*) *\\]\\]" :link)
          (first-instance-of "\{F(\\d+)\}" :file)
          (first-instance-of "T(\\d+)(#\\d+)?" :ticket)
          (first-instance-of "P(\\d+)(#\\d+)?" :snippet)
          (first-instance-of "D(\\d+)(#\\d+)?" :merge-request)))
        #'<
        :key #'car))))
    (when
     (and first-instance (equal :file (third first-instance)))
     (capture-file (fourth first-instance)))
    (cond
     ((zerop (length comment)) nil)
     ((not first-instance) (list comment))
     (t
      (append
       (when (not (zerop (car first-instance))) (list (subseq comment 0 (car first-instance))))
       (list (cddr first-instance))
       (parse-comment (subseq comment (cadr first-instance))))))))))

(defun convert-commit-to-core (commit)
 (cond
  ((repository-commit-commitidentifier commit)
   (forgerie-core:make-commit
    :sha (repository-commit-commitidentifier commit)
    :parsed-comment
    (when (repository-commit-git-comment commit)
     (parse-comment (repository-commit-git-comment commit)))))
  ((repository-commit-patch commit)
   (forgerie-core:make-patch :diff (repository-commit-patch commit)))))

(defun convert-change-comment-to-core (comment)
 (forgerie-core:make-merge-request-change-comment
  :old-line
  (when (zerop (differential-transaction-comment-isnewfile comment))
   (list
    (differential-transaction-comment-linenumber comment)
    (+ (differential-transaction-comment-linenumber comment) (differential-transaction-comment-linelength comment))))
  :new-line
  (when (not (zerop (differential-transaction-comment-isnewfile comment)))
   (list
    (differential-transaction-comment-linenumber comment)
    (+ (differential-transaction-comment-linenumber comment) (differential-transaction-comment-linelength comment))))
  :date (unix-to-universal-time (differential-transaction-comment-datecreated comment))
  :file (map 'string #'code-char (differential-diff-filename (differential-transaction-comment-diff comment)))
  :text (parse-comment (map 'string #'code-char (differential-transaction-comment-content comment)))
  :author (convert-user-to-core (differential-transaction-comment-author comment))
  :replies (mapcar #'convert-change-comment-to-core (differential-transaction-comment-replies comment))))

(defun convert-change-to-core (commit)
 (forgerie-core:make-merge-request-change
  :change (convert-commit-to-core commit)
  :comments (mapcar #'convert-change-comment-to-core (repository-commit-comments commit))))

(defun convert-differential-comment-to-core (comment)
 (forgerie-core:make-note
  :id (format nil "D~A" (differential-comment-id comment))
  :text (parse-comment (map 'string #'code-char (differential-comment-content comment)))
  :author (convert-user-to-core (differential-comment-author comment))
  :date (unix-to-universal-time (differential-comment-datecreated comment))))

(defun convert-revision-to-core (revision-def)
 (let
  ((type
    (cond
     ((find (differential-revision-status revision-def) (list "published" "abandoned") :test #'string=)
      :closed)
     ((find (differential-revision-status revision-def) (list "changes-planned" "needs-review" "needs-revision" "accepted" "draft") :test #'string=)
      :open)
     (t (error "Unknown revision type: ~A" (differential-revision-status revision-def))))))

  (forgerie-core:make-merge-request
   :id (differential-revision-id revision-def)
   :title (differential-revision-title revision-def)
   :description
   (parse-comment
    (format nil "~A~A"
     (map 'string #'code-char (differential-revision-summary revision-def))
     (if (differential-revision-testplan revision-def)
      (format nil "~%~%== Test Plan ==~%~%~A" (map 'string #'code-char (differential-revision-testplan revision-def)))
      "")))
   :author (convert-user-to-core (differential-revision-author revision-def))
   :vc-repository (convert-repository-to-core (differential-revision-repository revision-def))
   :date (unix-to-universal-time (differential-revision-datecreated revision-def))
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
   :changes (mapcar #'convert-change-to-core (differential-revision-related-commits revision-def))
   :other-change-comments (mapcar #'convert-change-comment-to-core (differential-revision-change-comments revision-def))
   :notes (mapcar #'convert-differential-comment-to-core (differential-revision-comments revision-def)))))

(defun convert-repository-to-core (repository-def)
 (forgerie-core:make-vc-repository
  :name (repository-name repository-def)
  :slug (repository-repositoryslug repository-def)
  :projects (mapcar #'convert-project-to-core (repository-projects repository-def))
  :primary-projects (mapcar #'convert-project-to-core (repository-primary-projects repository-def))
  :git-location (repository-localpath repository-def)
  :private (not (not (find (repository-spacephid repository-def) *confidential-space-phids* :test #'string=)))
  :commits (mapcar #'convert-commit-to-core (repository-commits repository-def))))

(defun convert-project-to-core (project-def)
 (forgerie-core:make-project
  :tags (project-tags project-def)
  :name (project-name project-def)))

(defun convert-email-to-core (email-def)
 (forgerie-core:make-email
  :address (sanitize-address (email-address email-def))
  :is-primary (eql (email-isprimary email-def) 1)))

(defun convert-user-to-core (user-def)
 (when user-def
  (forgerie-core:make-user
   :username (user-username user-def)
   :name (user-realname user-def)
   :admin (equal (user-isadmin user-def) 1)
   :emails (mapcar #'convert-email-to-core (user-emails user-def))
   :avatar (when (user-profileimage user-def) (convert-file-to-core (user-profileimage user-def))))))

(defun convert-task-comment-to-core (comment)
 (forgerie-core:make-note
  :id (format nil "T~A" (task-comment-id comment))
  :text (parse-comment (map 'string #'code-char (task-comment-content comment)))
  :author (convert-user-to-core (task-comment-author comment))
  :date (unix-to-universal-time (task-comment-datecreated comment))))

(defun convert-task-to-core (task-def)
 (let
  ((type
    (cond
     ((find (task-status task-def) (list "open" "wip") :test #'string=)
      :open)
     ((find (task-status task-def) (list "duplicate" "invalid" "resolved" "spite" "wontfix") :test #'string=)
      :closed)
     (t (error "Unknown revision type: ~A" (differential-revision-status revision-def))))))
  (forgerie-core:make-ticket
   :id (task-id task-def)
   :title (task-title task-def)
   :author (convert-user-to-core (task-author task-def))
   :assignee (convert-user-to-core (task-owner task-def))
   :description (parse-comment (map 'string #'code-char (task-description task-def)))
   :projects (mapcar #'convert-project-to-core (task-projects task-def))
   :date (unix-to-universal-time (task-datecreated task-def))
   :confidential (not (not (find (task-spacephid task-def) *confidential-space-phids* :test #'string=)))
   :linked-tickets (mapcar #'convert-task-to-core (task-linked-tasks task-def))
   :subscribers (mapcar #'convert-user-to-core (task-subscribers task-def))
   :priority
   (case (task-priority task-def)
    (100 "Unbreak!")
    (90 "Triage")
    (80 "High")
    (50 "Normal")
    (25 "Low")
    (0 "Wish"))
   :type type
   :notes (mapcar #'convert-task-comment-to-core (task-comments task-def)))))

(defun convert-paste-comment-to-core (comment)
 (forgerie-core:make-note
  :id (format nil "P~A" (paste-comment-id comment))
  :text (parse-comment (map 'string #'code-char (paste-comment-content comment)))
  :author (convert-user-to-core (paste-comment-author comment))
  :date (unix-to-universal-time (paste-comment-datecreated comment))))

(defun convert-file-to-core (file-def)
 (forgerie-core:make-file
  :id (file-id file-def)
  :name (file-name file-def)
  :location (file-location file-def)
  :size (file-bytesize file-def)
  :mimetype (file-mimetype file-def)))

(defun convert-paste-to-core (paste-def)
 (forgerie-core:make-snippet
  :id (paste-id paste-def)
  :date (unix-to-universal-time (paste-datecreated paste-def))
  :title (paste-title paste-def)
  :files (list (convert-file-to-core (paste-file paste-def)))
  :author (convert-user-to-core (paste-author paste-def))
  :notes (mapcar #'convert-paste-comment-to-core (paste-comments paste-def))))

(defmethod forgerie-core:import-forge ((forge (eql :phabricator)))
 (setf *working-directory* (format nil "~Aphabricator" forgerie-core:*working-directory*))
 (let
  ((override-everything-cache
    (and
     *included-repositories*
     (not
      (equal
       *included-repositories*
       (cached "everything" "included-repositories" nil))))))
  (cached "everything" "included-repositories" *included-repositories* t)
  (initialize)
  (list
   :users (cached "everything" "users" (mapcar #'convert-user-to-core (get-users)) override-everything-cache)
   :projects (cached "everything" "projects" (mapcar #'convert-project-to-core (get-projects)) override-everything-cache)
   :vc-repositories (cached "everything" "repositories" (mapcar #'convert-repository-to-core (get-repositories)) override-everything-cache)
   :snippets (cached "everything" "snippets" (mapcar #'convert-paste-to-core (get-pastes)) override-everything-cache)
   :merge-requests (cached "everything" "merge-requests" (mapcar #'convert-revision-to-core (get-revisions)) override-everything-cache)
   :tickets (cached "everything" "tickets" (mapcar #'convert-task-to-core (get-tasks)) override-everything-cache)
   :files (cached "everything" "files" (mapcar #'convert-file-to-core (get-captured-files)) override-everything-cache))))
