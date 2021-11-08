(in-package #:forgerie-gitlab)

(define-condition unknown-note-mapping
 nil
 ((mapping :initarg :mapping :reader unknown-note-mapping-mapping)))

(defvar *note-mapping-skips* nil)
(defvar *notes-mode* nil)
(defvar *files-to-upload* nil)

(defun validate-vc-repositories (vc-repositories projects)
 (let
  ((valid-projects
    (mapcar
     (lambda (proj)
      (let
       ((repos-for-proj (forgerie-core:vc-repositories-with-primary-project proj vc-repositories)))
       (cond
        ((< 1 (length repos-for-proj))
         (forgerie-core:add-mapping-error
          :gitlab-project-primary-in-multiple
          (forgerie-core:project-name proj)
          (format nil
           "Project ~A is the primary project in multiple repositories, and those repositories won't be included:~%~{ * ~A~%~}"
           (forgerie-core:project-name proj)
           (mapcar #'forgerie-core:vc-repository-name repos-for-proj)))
         nil)
        (proj))))
     projects)))
  (remove
   nil
   (mapcar
    (lambda (vcr)
     (cond
      ((cl-ppcre:scan "[,()/+]" (forgerie-core:vc-repository-name vcr))
       (forgerie-core:add-mapping-error
        :gitlab-repository-has-illegal-name
        (forgerie-core:vc-repository-name vcr)
        (format nil "VC Repository '~A' has an illegal name due to an illegal character, one of: ',()/+'." (forgerie-core:vc-repository-name vcr))))
      ((cl-ppcre:scan "^ " (forgerie-core:vc-repository-name vcr))
       (forgerie-core:add-mapping-error
        :gitlab-repository-has-illegal-name
        (forgerie-core:vc-repository-name vcr)
        (format nil "VC Repository '~A' has an illegal name due to starting with a space." (forgerie-core:vc-repository-name vcr))))
      ((not (forgerie-core:vc-repository-primary-projects vcr))
       (forgerie-core:add-mapping-error
        :gitlab-repository-has-no-projects
        (forgerie-core:vc-repository-name vcr)
        (format nil "VC Repository '~A' has no primary projects.~%" (forgerie-core:vc-repository-name vcr)))
       vcr)
      ((not
        (remove-if-not
         (lambda (proj) (find proj valid-projects :test #'equalp))
         (forgerie-core:vc-repository-primary-projects vcr)))
       nil)
      (vcr)))
    vc-repositories))))

(defun validate-users (users)
 (remove nil
  (mapcar
   (lambda (user)
    (cond
     ((< (length (forgerie-core:user-username user)) 2)
      (forgerie-core:add-mapping-error
       :gitlab-username-too-short
       (forgerie-core:user-username user)
       (format nil "User '~A' (~{~A~^,~}) has too short of a username."
        (forgerie-core:user-username user)
        (mapcar #'forgerie-core:email-address (forgerie-core:user-emails user)))))
     (user)))
   users)))

(defun ticket-assignable-vc-repositories (ticket vc-repositories)
 (when (forgerie-core:ticket-projects ticket)
  (remove
   nil
   (remove-duplicates
    (apply #'append
     (mapcar
      (lambda (proj) (forgerie-core:vc-repositories-with-primary-project proj vc-repositories))
      (forgerie-core:ticket-projects ticket)))
    :test #'equalp))))

; This assumes that validate-vc-repositories passed, which is to say
; that every project of interest belongs to only one repository, and that
; every vc-repository has at least one primary project
(defun validate-tickets (tickets vc-repositories)
 (remove
  nil
  (mapcar
   (lambda (ticket)
    (let
     ((vc-repos (ticket-assignable-vc-repositories ticket vc-repositories)))
     (cond
      ((not vc-repos)
       (forgerie-core:add-mapping-error
        :gitlab-ticket-assigned-to-default
        (forgerie-core:ticket-id ticket)
        (format nil "Ticket with id ~A is not assignable to a repository, so assigning to default" (forgerie-core:ticket-id ticket)))
       ticket)
      ((< 1 (length vc-repos))
       (forgerie-core:add-mapping-error
        :gitlab-ticket-assigned-to-multiple
        (forgerie-core:ticket-id ticket)
        (format nil
         "Ticket with id ~A is assignable to multiple repositories:~%~{ * ~A~%~}"
         (forgerie-core:ticket-id ticket)
         (mapcar #'forgerie-core:vc-repository-name vc-repos)))
       nil)
      (ticket))))
   tickets)))

(defun validate-merge-requests (merge-requests vc-repositories)
 (remove
  nil
  (mapcar
   (lambda (mr)
    (if
     (not (find (forgerie-core:merge-request-vc-repository mr) vc-repositories :test #'equalp))
     (forgerie-core:add-mapping-error
      :gitlab-merge-request-not-assignable
      (forgerie-core:merge-request-id mr)
      (format nil "Merge Request with title ~A is not assignable to a repository~%" (forgerie-core:merge-request-title mr)))
     mr))
   merge-requests)))

; We only cache this in memory, and not on disk, because we most likely want
; updated information any time a run is fresh.
(defvar *projects-by-name* nil)
(defvar *projects-by-id* nil)

(defun find-project-by-name (name)
 (when (not (assoc name *projects-by-name* :test #'string=))
  (let
   ((project 
     (find
      name
      (get-request "projects" :parameters `(("search" . ,name)))
      :test #'string=
      :key (lambda (gl-project) (getf gl-project :name)))))
   (setf *projects-by-name* (cons (cons name project) *projects-by-name*))
   (setf *projects-by-id* (cons (cons (getf project :id) project) *projects-by-id*))))
 (cdr (assoc name *projects-by-name* :test #'string=)))

(defun find-project-by-id (id)
 (when (not (assoc id *projects-by-id*))
  (let
   ((project (get-request (format nil "projects/~A" id))))
   (setf *projects-by-id* (cons (cons (getf project :id) project) *projects-by-id*))))
 (cdr (assoc id *projects-by-id*)))

(defun default-project ()
 (find-project-by-name (getf *default-project* :name)))

(defun create-default-project ()
 (when-unmapped-with-update (:project :default-project)
  (post-request
   "projects"
   (append
    (when *default-group*
     (list
      (cons
       "namespace_id"
       (princ-to-string (getf (first (get-request "namespaces" :parameters `(("search" . ,(getf *default-group* :name))))) :id)))))
    `(("name" . ,(getf *default-project* :name))
      ("issues_access_level" . "enabled")
      ("snippets_access_level" . "enabled")
      ("path" . ,(getf *default-project* :path)))))))

(defun default-group ()
 (when *default-group*
  (get-request
   "groups"
   :parameters `(("search" . ,(getf *default-group* :name))))))

(defun create-default-group ()
 (when *default-group*
  (when-unmapped-with-update (:group :default-group)
   (post-request
    "groups"
    `(("name" . ,(getf *default-group* :name))
      ("path" . ,(getf *default-group* :path)))))))

(defun add-ssh-key ()
 (let
  ((key-name "Forgerie Export Key"))
  (when-unmapped-with-update (:forgerie-key :main-key)
   (post-request
    "user/keys"
    `(("title" . ,key-name)
      ("key" . ,*ssh-public-key*))))))

(defun project-for-ticket (ticket vc-repositories)
 (let
  ((vc-repos (ticket-assignable-vc-repositories ticket vc-repositories)))
  (if vc-repos
   (find-project-by-name (forgerie-core:vc-repository-name (car vc-repos)))
   (default-project))))

(defun remove-single-project ()
 (when *single-project*
  (let
   ((project (find-project-by-name *single-project*)))
   (when project
    (cl-fad:delete-directory-and-files
     (format nil "~A~A/" *working-directory* (getf project :path))
     :if-does-not-exist :ignore)
    (delete-request (format nil "/projects/~A" (getf project :id)))
    (setf *projects-by-name* nil)
    ; Gitlab returns immediately even though the project is being deleted....
    (sleep 60)))))

(defmethod forgerie-core:export-forge ((forge (eql :gitlab)) data)
 (ensure-directories-exist *working-directory*)
 (when *single-project* (remove-single-project))
 (create-default-group)
 (create-default-project)
 (add-ssh-key)
 (let*
  ((*note-mapping-skips* nil)
   (*notes-mode* nil)
   (*files-to-upload* (getf data :files))
   (vc-repositories (validate-vc-repositories (getf data :vc-repositories) (getf data :projects)))
   (tickets (remove-if-not #'identity (validate-tickets (getf data :tickets) vc-repositories)))
   (merge-requests (validate-merge-requests (getf data :merge-requests) vc-repositories)))
  (mapcar #'create-user (validate-users (getf data :users)))
  (mapcar #'create-project vc-repositories)
  (loop
   :with moved-forward := t
   :with completed := nil
   :with first-error := nil
   :with number-of-errors := 0
   :while moved-forward
   :do
   (flet
    ((map-with-note-mapping-catch (fn collection)
      (mapcar
       (lambda (item)
        (let
         ((item-info
           (list
             (type-of item)
             (typecase item
              (forgerie-core:ticket (forgerie-core:ticket-id item))
              (forgerie-core:merge-request (forgerie-core:merge-request-id item))
              (forgerie-core:snippet (forgerie-core:snippet-id item))))))
         (when (not (find item completed :test #'equalp))
          (handler-case
           (progn
            (funcall fn item)
            (setf moved-forward t)
            (setf completed (cons item completed)))
           (unknown-note-mapping (e)
            (incf number-of-errors)
            (when (not first-error) (setf first-error (unknown-note-mapping-mapping e))))))))
       collection)))
     (setf moved-forward nil)
     (setf first-error nil)
     (setf number-of-errors 0)
     (map-with-note-mapping-catch (lambda (ticket) (create-ticket ticket vc-repositories)) tickets)
     (map-with-note-mapping-catch #'create-snippet (getf data :snippets))
     (map-with-note-mapping-catch #'create-merge-request merge-requests)
     (when (and (not first-error) (not *notes-mode*))
      (setf *notes-mode* t)
      (setf completed nil)
      (setf moved-forward t))
     (when (and (not moved-forward) first-error)
      (when forgerie-core:*debug* (format t "We failed to move forward...., so skipping item ~A~%" first-error))
      (setf moved-forward t)
      (push first-error *note-mapping-skips*))))
  (mapcar (lambda (ticket) (create-ticket-links ticket vc-repositories)) tickets)
  (mapcar #'update-user-admin-status (validate-users (getf data :users)))))

; Projects are created from vc repositories, since they are linked in gitlab.
; Some of the underlying information comes from core:projects that are
; the primary projects of the vc-repository
(defun create-project (vc-repository)
 (single-project-check (forgerie-core:vc-repository-name vc-repository)
  (when-unmapped (:project (forgerie-core:vc-repository-slug vc-repository))
   (let*
    ((tags
      (remove-duplicates
       (apply #'append
        (mapcar #'forgerie-core:project-tags (forgerie-core:vc-repository-projects vc-repository)))
       :test #'string=))
     (gl-project
      (post-request
       "projects"
       (append
        (when *default-group*
         (list
          (cons
           "namespace_id"
           (princ-to-string (getf (first (get-request "namespaces" :parameters `(("search" . ,(getf *default-group* :name))))) :id)))))
       `(("name" . ,(forgerie-core:vc-repository-name vc-repository))
         ("path" . ,(forgerie-core:vc-repository-slug vc-repository))
         ("tag_list" . ,(format nil "~{~A~^,~}" tags))
         ("issues_access_level" . "enabled")
         ("merge_requests_access_level" . "enabled")))))
     (working-path (format nil "~A~A/" *working-directory* (getf gl-project :path))))
    (when
     (getf gl-project :empty_repo)
     (ensure-directories-exist working-path)
     (git-cmd gl-project "clone" "--mirror" (forgerie-core:vc-repository-git-location vc-repository) ".")
     (git-cmd gl-project "remote" "add" "gitlab" (getf gl-project :ssh_url_to_repo))
     (git-cmd gl-project "push" "gitlab" "--all")
     (git-cmd gl-project "push" "gitlab" "--tags")
     (uiop/filesystem:delete-directory-tree (pathname working-path) :validate t)
     (update-mapping (:project (forgerie-core:vc-repository-slug vc-repository)) gl-project))))))

(defun process-note-text (note-text project-id)
 (format nil "~{~A~}"
  (mapcar
   (lambda (item)
    (flet
     ((mapped-item-p (item type) (and (eql type (car item)) (find-mapped-item type (parse-integer (cadr item)))))
      (handle-mapped-item (item type c)
       (let
        ((mi (find-mapped-item type (parse-integer (cadr item)))))
        (if (equal project-id (mapped-item-project-id mi))
         (format nil "~A~A" c (or (mapped-item-iid mi) (mapped-item-id mi)))
         (let
          ((other-project (find-project-by-id (mapped-item-project-id mi))))
          (format nil "~A~A~A" (getf other-project :path) c (or (mapped-item-iid mi) (mapped-item-id mi)))))))
      (handle-file (file-id)
       (let
        ((file-response (create-file file-id project-id)))
        (getf file-response :markdown))))
     (cond
      ((stringp item) item)
      ((eql (car item) :file) (handle-file (cadr item)))
      ((mapped-item-p item :ticket) (handle-mapped-item item :ticket "#"))
      ((mapped-item-p item :merge-request) (handle-mapped-item item :merge-request "!"))
      ((mapped-item-p item :snippet) (handle-mapped-item item :snippet "$"))
      ((find item *note-mapping-skips* :test #'equalp)
       (caddr item))
      (*notes-mode* (caddr item))
      (t (error (make-instance 'unknown-note-mapping :mapping item))))))
   note-text)))

(defun create-note (project-id item-type item-id note)
 (when *notes-mode*
  (let
   ((note-text (process-note-text (forgerie-core:note-text note) project-id)))
   (when
    (not (cl-ppcre:scan "^\\s*$" note-text))
    (when-unmapped-with-update (:note (forgerie-core:note-id note))
     (post-request
      (format nil "/~A~A/~A/notes"
       (if project-id (format nil "projects/~A/" project-id) "") item-type item-id)
     `(("body" . ,note-text)
       ("created_at" . ,(to-iso-8601 (forgerie-core:note-date note))))
      :sudo (forgerie-core:user-username (forgerie-core:note-author note))))))))

(defun create-file (file-id project-id)
 (let
  ((file (find (parse-integer file-id) *files-to-upload* :key #'forgerie-core:file-id)))
  (when (not file)
   (error (format nil "Couldn't find file to upload with id ~S" (parse-integer file-id))))
  (when-unmapped (:file-upoaded (forgerie-core:file-id file))
   (update-file-mapping (:file-upoaded (forgerie-core:file-id file))
    (let
     ((data
       (if (stringp (forgerie-core:file-data file))
        (map 'vector #'char-code (forgerie-core:file-data file))
        (forgerie-core:file-data file))))
     (flexi-streams:with-input-from-sequence (str data)
      (post-request
       (format nil "projects/~A/uploads" project-id)
       `(("file" . ,(list str :filename (drakma:url-encode (forgerie-core:file-name file) :utf-8)))))))))
  (retrieve-mapping :file-upoaded (forgerie-core:file-id file))))

(defun note-mapped (note)
 (find-mapped-item :find-mapped-item (forgerie-core:note-id note)))

(defun create-ticket (ticket vc-repositories)
 (single-project-check
  (let
   ((vc-repos (ticket-assignable-vc-repositories ticket vc-repositories)))
   (if vc-repos (forgerie-core:vc-repository-name (car vc-repos)) (getf *default-project* :name)))
  (when-unmapped (:ticket-completed (forgerie-core:ticket-id ticket))
   (let
    ((project-id (getf (project-for-ticket ticket vc-repositories) :id)))
    (when-unmapped (:ticket (forgerie-core:ticket-id ticket))
     (let
      ((gl-ticket
        (post-request
         (format nil "projects/~A/issues" project-id)
         `(("iid" . ,(prin1-to-string (forgerie-core:ticket-id ticket)))
           ("title" . ,(forgerie-core:ticket-title ticket))
           ("labels" .
            ,(format nil "~{~A~^,~}"
              (cons
               (format nil "priority:~A" (forgerie-core:ticket-priority ticket))
               (mapcar #'forgerie-core:project-name (forgerie-core:ticket-projects ticket)))))
         ,@(when (forgerie-core:ticket-assignee ticket)
            (list (cons "assignee_id" (princ-to-string (getf (retrieve-mapping :user (forgerie-core:user-username (forgerie-core:ticket-assignee ticket))) :id)))))
           ("confidential" . ,(if (forgerie-core:ticket-confidential ticket) "true" "false"))
           ("description" . ,(process-note-text (forgerie-core:ticket-description ticket) project-id))
           ("created_at" . ,(to-iso-8601 (forgerie-core:ticket-date ticket))))
         :sudo (forgerie-core:user-username (forgerie-core:ticket-author ticket)))))
      (mapcar
       (lambda (u)
        (post-request
         (format nil "projects/~A/issues/~A/subscribe" (getf gl-ticket :project_id) (getf gl-ticket :iid))
         nil
         :sudo (forgerie-core:user-username u)))
       (forgerie-core:ticket-subscribers ticket))
      (update-mapping (:ticket (forgerie-core:ticket-id ticket)) gl-ticket)))
   (when
    (and
     *notes-mode*
     (notevery #'identity (mapcar #'note-mapped (forgerie-core:ticket-notes ticket))))
    (let
     ((gl-ticket (get-request (format nil "projects/~A/issues/~A" project-id (forgerie-core:ticket-id ticket)))))
     (mapcar
      (lambda (note)
       (create-note (getf gl-ticket :project_id) "issues" (getf gl-ticket :iid) note))
      (forgerie-core:ticket-notes ticket))
     (when (eql :closed (forgerie-core:ticket-type ticket))
      (put-request
       (format nil "projects/~A/issues/~A" project-id (getf gl-ticket :iid))
       '(("state_event" . "close"))))
     (update-mapping (:ticket-completed (forgerie-core:ticket-id ticket)))))))))

(defun create-ticket-links (ticket vc-repositories)
 (single-project-check
  (let
   ((vc-repos (ticket-assignable-vc-repositories ticket vc-repositories)))
   (if vc-repos (forgerie-core:vc-repository-name (car vc-repos)) (getf *default-project* :name)))
  (let
   ((gl-ticket (retrieve-mapping :ticket (forgerie-core:ticket-id ticket))))
   (mapcar
    (lambda (linked-ticket)
     (let
      ((gl-linked-ticket (ignore-errors (retrieve-mapping :ticket (forgerie-core:ticket-id linked-ticket)))))
      (if (not gl-linked-ticket)
       (forgerie-core:add-mapping-error
        :linked-ticket-not-found
        (forgerie-core:ticket-id linked-ticket)
        (format nil "Link was between ~A and ~A" (forgerie-core:ticket-id ticket) (forgerie-core:ticket-id linked-ticket)))
       (post-request
        (format nil "projects/~A/issues/~A/links" (getf gl-ticket :project_id) (getf gl-ticket :iid))
        `(("target_project_id" . ,(princ-to-string (getf gl-linked-ticket :project_id)))
          ("target_issue_iid" . ,(princ-to-string (getf gl-linked-ticket :iid))))))))
    (forgerie-core:ticket-linked-tickets ticket)))))

(defun create-user (user)
 (when-unmapped-with-update (:user (forgerie-core:user-username user))
  (let
   ((gl-user
     (post-request
      "users"
      `(("name" . ,(forgerie-core:user-name user))
        ("email" . ,(forgerie-core:email-address (forgerie-core:user-primary-email user)))
        ; Everyone must be an admin to make some of the other import things work correctly
        ; and then admin must be removed after
        ("admin" . "true")
        ("reset_password" . "true")
        ("username" . ,(forgerie-core:user-username user))))))
   (mapcar
    (lambda (email)
     (post-request (format nil "/users/~A/emails" (getf gl-user :id))
      `(("email" . ,(forgerie-core:email-address email)))))
    (remove-if #'forgerie-core:email-is-primary (forgerie-core:user-emails user)))
   gl-user)))

(defun update-user-admin-status (user)
 (when-unmapped (:user-admin-set (forgerie-core:user-username user))
  (let
   ((gl-user (retrieve-mapping :user (forgerie-core:user-username user))))
   (put-request
    (format nil "/users/~A" (getf gl-user :id))
    `(("admin" . ,(if (forgerie-core:user-admin user) "true" "false")))))
  (update-mapping (:user-admin-set (forgerie-core:user-username user)))))

(defun create-local-checkout (project)
 (when (not (probe-file (format nil "~A~A" *working-directory* (getf project :path))))
  (ensure-directories-exist (format nil "~A~A/" *working-directory* (getf project :path)))
  (git-cmd project "clone" "-o" "gitlab" (getf project :ssh_url_to_repo) ".")))

(defun create-change-comments (gl-mr change)
 (let*
  ((versions (get-request (format nil "/projects/~A/merge_requests/~A/versions" (getf gl-mr :project_id) (getf gl-mr :iid))))

   ; This may not work!  We may have to figure out how to correlate version with this commit
   (version-for-change (car versions)))

  (mapcar
   (lambda (comment)
    (let
     ((note-text (process-note-text (forgerie-core:merge-request-change-comment-text comment) (getf gl-mr :project_id))))
     (when
      (and note-text (not (zerop (length note-text))))
      (handler-case
       (let
        ((discussion
          (post-request
           (format nil "/projects/~A/merge_requests/~A/discussions" (getf gl-mr :project_id) (getf gl-mr :iid))
           `(("position[position_type]" . "text")
             ("position[base_sha]" . ,(getf version-for-change :base_commit_sha))
             ("position[head_sha]" . ,(getf version-for-change :head_commit_sha))
             ("position[start_sha]" . ,(getf version-for-change :start_commit_sha))
             ;("position[line_range][start][line_code]" . "40606d8fa72800ddf68b5f2cf2b0b30e1d2de8e2_224_131")
             ;("position[line_range][start][type]" . "new")
             ;("position[line_range][start][new_line]" . "131")
             ;("position[line_range][end][line_code]" . "40606d8fa72800ddf68b5f2cf2b0b30e1d2de8e2_224_134")
             ;("position[line_range][end][type]" . "new")
             ;("position[line_range][end][new_line]" . "134")
             ("position[new_line]" . ,(princ-to-string (forgerie-core:merge-request-change-comment-line comment)))
             ("position[old_path]" . ,(forgerie-core:merge-request-change-comment-file comment))
             ("position[new_path]" . ,(forgerie-core:merge-request-change-comment-file comment))
             ("body" . ,note-text)
             ("created_at" . ,(to-iso-8601 (forgerie-core:merge-request-change-comment-date comment))))
           :sudo (forgerie-core:user-username (forgerie-core:merge-request-change-comment-author comment)))))
        (mapcar
         (lambda (comment)
          (let
           ((note-text (process-note-text (forgerie-core:merge-request-change-comment-text comment) (getf gl-mr :project_id))))
           (when
            (and note-text (not (zerop (length note-text))))
            (post-request
             (format nil "/projects/~A/merge_requests/~A/discussions/~A/notes" (getf gl-mr :project_id) (getf gl-mr :iid) (getf discussion :id))
             `(("body" . ,note-text)
               ("created_at" . ,(to-iso-8601 (forgerie-core:merge-request-change-comment-date comment))))
             :sudo (forgerie-core:user-username (forgerie-core:merge-request-change-comment-author comment))))))
         (forgerie-core:merge-request-change-comment-replies comment)))
       (http-error (e)
        (cond
         ((= 400 (http-error-code e))
          (format *standard-output* "400 error in create-change-comments: ~A~%" (http-error-resp e)))
         ((= 500 (http-error-code e))
          (format *standard-output* "500 error in create-change-comments: ~A~%" (http-error-resp e)))
         (t (error e))))))))
   (forgerie-core:merge-request-change-comments change))))

(defun create-merge-request (mr)
 (single-project-check
  (forgerie-core:vc-repository-name (forgerie-core:merge-request-vc-repository mr))
  (when-unmapped (:merge-request-completed (forgerie-core:merge-request-id mr))
   (let*
    ((project-name
      (forgerie-core:vc-repository-name
       (forgerie-core:merge-request-vc-repository
        mr)))
     (project (find-project-by-name project-name)))
    (when-unmapped (:merge-request (forgerie-core:merge-request-id mr))
     (when (not project)
      (error "Could not find project with name: ~A" project-name))
     (create-local-checkout project)
     ; We do this first, because if this errors, we want to bomb out first without doing the work
     ; to create all the branches and whatnot.  The other option would be to add a mapping for
     ; the git work we need to do, but this seemed more elegant.
     (process-note-text (forgerie-core:merge-request-description mr) (getf project :id))
     (when
      (not
       (zerop
        (git-cmd-code project "show-ref" "--verify" "--quiet"
         (format nil "refs/heads/~A" (forgerie-core:branch-name (forgerie-core:merge-request-source-branch mr))))))
      (git-cmd project "branch"
       (forgerie-core:branch-name (forgerie-core:merge-request-source-branch mr))
       (forgerie-core:commit-sha (forgerie-core:branch-commit (forgerie-core:merge-request-source-branch mr)))))
     (when
      (not
       (zerop
        (git-cmd-code project "show-ref" "--verify" "--quiet"
         (format nil "refs/heads/~A" (forgerie-core:branch-name (forgerie-core:merge-request-target-branch mr))))))
      (git-cmd project "branch"
       (forgerie-core:branch-name (forgerie-core:merge-request-target-branch mr))
       (forgerie-core:commit-sha (forgerie-core:branch-commit (forgerie-core:merge-request-source-branch mr)))))
     (git-cmd project "checkout"
      (forgerie-core:branch-name (forgerie-core:merge-request-source-branch mr)))
     (mapcar
      (lambda (change)
       (let
        ((commit (forgerie-core:merge-request-change-change change)))
        (typecase commit
         (forgerie-core:commit (git-cmd project "merge" (forgerie-core:commit-sha commit)))
         (forgerie-core:patch
          (let
           ((patch-file (format nil "~A/working.patch" *working-directory*)))
           (with-open-file (str patch-file :direction :output :if-exists :supersede :if-does-not-exist :create)
            (princ (forgerie-core:patch-diff commit) str))
           (git-cmd project "am" patch-file)
           (delete-file patch-file))))))
      (forgerie-core:merge-request-changes mr))
     (git-cmd project "push" "gitlab" (forgerie-core:branch-name (forgerie-core:merge-request-source-branch mr)))
     (git-cmd project "push" "gitlab" (forgerie-core:branch-name (forgerie-core:merge-request-target-branch mr)))
     (update-mapping (:merge-request (forgerie-core:merge-request-id mr))
      (post-request
       (format nil "projects/~A/merge_requests" (getf project :id))
       `(("source_branch" . ,(forgerie-core:branch-name (forgerie-core:merge-request-source-branch mr)))
         ("target_branch" . ,(forgerie-core:branch-name (forgerie-core:merge-request-target-branch mr)))
         ("description" . ,(process-note-text (append (forgerie-core:merge-request-description mr) (list (merge-request-suffix mr))) (getf project :id)))
         ("title" . ,(forgerie-core:merge-request-title mr)))
       :sudo (forgerie-core:user-username (forgerie-core:merge-request-author mr)))))
   (when *notes-mode*
    (let
     ((gl-mr (retrieve-mapping :merge-request (forgerie-core:merge-request-id mr))))
     (rails-command (format nil "mr = MergeRequest.find(~A)" (getf gl-mr :id)))
     (rails-command (format nil "mr.created_at = Time.parse(\"~A\")" (to-iso-8601 (forgerie-core:merge-request-date mr))))
     (rails-command "mr.save")
     (mapcar
      (lambda (note) (create-note (getf gl-mr :project_id) "merge_requests" (getf gl-mr :iid) note))
      (forgerie-core:merge-request-notes mr))
     (mapcar
      (lambda (change)
       (create-change-comments gl-mr change))
      (forgerie-core:merge-request-changes mr))
     (when (eql :closed (forgerie-core:merge-request-type mr))
      (put-request
       (format nil "projects/~A/merge_requests/~A" (getf project :id) (getf gl-mr :iid))
       '(("state_event" . "close")))
      (git-cmd project "push" "gitlab" "--delete" (forgerie-core:branch-name (forgerie-core:merge-request-source-branch mr)))
      (git-cmd project "push" "gitlab" "--delete" (forgerie-core:branch-name (forgerie-core:merge-request-target-branch mr))))
     (update-mapping (:merge-request-completed (forgerie-core:merge-request-id mr)))))))))

(defun create-snippet (snippet)
 (single-project-check (getf *default-project* :name)
  (when-unmapped (:snippet-completed (forgerie-core:snippet-id snippet))
   (when
    (/= 1 (length (forgerie-core:snippet-files snippet)))
    (error "Can only export snippets with exactly one file for now"))
   (let
    ((default-project (default-project))
     (file (first (forgerie-core:snippet-files snippet))))
    (if
     (zerop (length (forgerie-core:file-data file)))
     (forgerie-core:add-mapping-error
      :gitlab-snippet-empty
      (forgerie-core:snippet-id snippet)
      (format nil "Skipping snippet ~A because empty content" (forgerie-core:snippet-id snippet)))
     (progn
      (when-unmapped (:snippet (forgerie-core:snippet-id snippet))
       (handler-case
        (update-mapping (:snippet (forgerie-core:snippet-id snippet))
         (post-request
          (format nil "/projects/~A/snippets" (getf default-project :id))
          ; This is deprecated, but it's an easier interface for now.  Someday we may have
          ; an importer that has more than one file, or gitlab may fully remove this, and
          ; then this code will need to be updated
          ;
          ; See https://docs.gitlab.com/ee/api/snippets.html#create-new-snippet
         `(("title" . ,(or (forgerie-core:snippet-title snippet) "Forgerie Generated Title"))
           ("content" . ,(forgerie-core:file-data file))
           ("visibility" . "public")
           ("file_name" . ,(forgerie-core:file-name file)))))
        (error (e)
         (forgerie-core:add-mapping-error
          :gitlab-snippet-error
          (forgerie-core:snippet-id snippet)
          (format nil "Failed to create snippet with title ~A, due to error ~A" (forgerie-core:snippet-title snippet) e)))))
      (when *notes-mode*
       (let
        ((gl-snippet (retrieve-mapping :snippet (forgerie-core:snippet-id snippet))))
        (list
         gl-snippet
         (mapcar
          (lambda (note) (create-note (getf default-project :id) "snippets" (getf gl-snippet :id) note))
          (forgerie-core:snippet-notes snippet)))
        (rails-command (format nil "s = Snippet.find(~A)" (getf gl-snippet :id)))
        (rails-command (format nil "u = User.find_by_username(\"~A\")" (forgerie-core:user-username (forgerie-core:snippet-author snippet))))
        (rails-command "s.author = u")
        (rails-command "s.save")
        (update-mapping (:snippet-completed (forgerie-core:snippet-id snippet)) gl-snippet)))))))))
