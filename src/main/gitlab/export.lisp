(in-package #:forgerie-gitlab)

(defun validate-vc-repositories (vc-repositories projects)
 (let
  ((valid-projects
    (mapcar
     (lambda (proj)
      (let
       ((repos-for-proj (forgerie-core:vc-repositories-with-primary-project proj vc-repositories)))
       (cond
        ((< 1 (length repos-for-proj))
         (format *standard-output*
          "Project ~A is the primary project in multiple repositories, and those repositories won't be included:~%~{ * ~A~%~}"
          (forgerie-core:project-name proj)
          (mapcar #'forgerie-core:vc-repository-name repos-for-proj))
         nil)
        (proj))))
     projects)))
  (remove
   nil
   (mapcar
    (lambda (vcr)
     (cond
      ((cl-ppcre:scan "[,()/+]" (forgerie-core:vc-repository-name vcr))
       (format *error-output* "VC Repository '~A' has an illegal name due to an illegal character, one of: ',()/+'.~%" (forgerie-core:vc-repository-name vcr)))
      ((cl-ppcre:scan "^ " (forgerie-core:vc-repository-name vcr))
       (format *error-output* "VC Repository '~A' has an illegal name due to starting with a space.~%" (forgerie-core:vc-repository-name vcr)))
      ((not (forgerie-core:vc-repository-primary-projects vcr))
       ; Note that this output is just for debugging purposes, it doesn't actually stop anything
       ; from hapening
       (format *error-output* "VC Repository '~A' has no primary projects.~%" (forgerie-core:vc-repository-name vcr))
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
      (format *error-output* "User '~A' (~{~A~^,~}) has too short of a username.  Skipping."
       (forgerie-core:user-username user)
       (mapcar #'forgerie-core:email-address (forgerie-core:user-emails user))))
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
       (format *error-output* "Ticket with id ~A is not assignable to a repository~%" (forgerie-core:ticket-id ticket))
       :not-assignable)
      ((< 1 (length vc-repos))
       (format *error-output*
        "Ticket with id ~A is assignable to multiple repositories:~%~{ * ~A~%~}"
        (forgerie-core:ticket-id ticket)
        (mapcar #'forgerie-core:vc-repository-name vc-repos))
       :multiple-assignable)
      (ticket))))
   tickets)))

(defun validate-merge-requests (merge-requests vc-repositories)
 (remove
  nil
  (mapcar
   (lambda (mr)
    (if
     (not (find (forgerie-core:merge-request-vc-repository mr) vc-repositories :test #'equalp))
     (format *error-output* "Merge Request with title ~A is not assignable to a repository~%" (forgerie-core:merge-request-title mr))
     mr))
   merge-requests)))

(defun find-project-by-name (name)
 (first
  (get-request
   "projects"
   ; This is a list of one item in order to get past validation
   :parameters `(("search" . ,name)))))

(defun default-project ()
 (find-project-by-name (getf *default-project* :name)))

(defun create-default-project ()
 (when-unmapped-with-update (:project :default-project)
  (post-request
   "projects"
   `(("name" . ,(getf *default-project* :name))
     ("issues_access_level" . "enabled")
     ("snippets_access_level" . "enabled")
     ("path" . ,(getf *default-project* :path))))))

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
 (find-project-by-name (forgerie-core:vc-repository-name (car (ticket-assignable-vc-repositories ticket vc-repositories)))))

(defmethod forgerie-core:export-forge ((forge (eql :gitlab)) data)
 (create-default-project)
 (create-default-group)
 (add-ssh-key)
 (let*
  ((vc-repositories (validate-vc-repositories (getf data :vc-repositories) (getf data :projects)))
   (tickets (remove-if #'keywordp (validate-tickets (getf data :tickets) vc-repositories)))
   (merge-requests (validate-merge-requests (getf data :merge-requests) vc-repositories)))
  (mapcar #'create-user (validate-users (getf data :users)))
  (mapcar #'create-project vc-repositories)
  (mapcar (lambda (ticket) (create-ticket ticket vc-repositories)) tickets)
  (mapcar #'create-snippet (getf data :snippets))
  (mapcar #'create-merge-request merge-requests)))

; Projects are created from vc repositories, since they are linked in gitlab.
; Some of the underlying information comes from core:projects that are
; the primary projects of the vc-repository
(defun create-project (vc-repository)
 (when-unmapped (:project (forgerie-core:vc-repository-slug vc-repository))
  (let*
   ((gl-project
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
        ("issues_access_level" . "enabled")
        ("merge_requests_access_level" . "enabled")))))
    (working-path (format nil "~A~A/" *working-directory* (getf gl-project :path))))
   (when
    (getf gl-project :empty_repo)
    (ensure-directories-exist working-path)
    (git-cmd gl-project "clone" "--mirror" (forgerie-core:vc-repository-git-location vc-repository) ".")
    (git-cmd gl-project "remote" "add" "gitlab" (getf gl-project :ssh_url_to_repo))
    (git-cmd gl-project "push" "gitlab" "--mirror")
    (uiop/filesystem:delete-directory-tree (pathname working-path) :validate t)
    (update-mapping (:project (forgerie-core:vc-repository-slug vc-repository)) gl-project)))))

(defun create-note (project-id item-type item-id note)
 (when
  (not (zerop (length (forgerie-core:note-text note))))
  (post-request
   (format nil "/~A~A/~A/notes"
    (if project-id (format nil "projects/~A/" project-id) "") item-type item-id)
  `(("body" . ,(forgerie-core:note-text note))
    ("created_at" . ,(to-iso-8601 (forgerie-core:note-date note))))
   :sudo (forgerie-core:user-username (forgerie-core:note-author note)))))

(defun create-ticket (ticket vc-repositories)
 (when-unmapped (:ticket (forgerie-core:ticket-id ticket))
  (let*
   ((project-id (getf (project-for-ticket ticket vc-repositories) :id))
    (ticket-id (prin1-to-string (forgerie-core:ticket-id ticket)))
    (gl-ticket
     (update-mapping (:ticket (forgerie-core:ticket-id ticket))
      (post-request
       (format nil "projects/~A/issues" project-id)
       `(("iid" . ,(prin1-to-string (forgerie-core:ticket-id ticket)))
         ("title" . ,(forgerie-core:ticket-title ticket)))))))
   (mapcar
    (lambda (note)
     (create-note (getf gl-ticket :project_id) "issues" (getf gl-ticket :iid) note))
    (forgerie-core:ticket-notes ticket)))))

(defun create-user (user)
 (when-unmapped-with-update (:user (forgerie-core:user-username user))
  (post-request
   "users"
   `(("name" . ,(forgerie-core:user-name user))
     ("email" . ,(forgerie-core:email-address (forgerie-core:user-primary-email user)))
     ; Everyone must be an admin to make some of the other import things work correctly
     ; and then admin must be removed after
     ("admin" . "true")
     ("reset_password" . "true")
     ("username" . ,(forgerie-core:user-username user))))))

(defun create-local-checkout (project)
 (when (not (probe-file (format nil "~A~A" *working-directory* (getf project :path))))
  (ensure-directories-exist (format nil "~A~A/" *working-directory* (getf project :path)))
  (git-cmd project "clone" "-o" "gitlab" (getf project :ssh_url_to_repo) ".")))

(defun create-merge-request (mr)
 (when-unmapped (:merge-request (forgerie-core:merge-request-id mr))
  (let*
   ((project-name
     (forgerie-core:vc-repository-name
      (forgerie-core:merge-request-vc-repository
       mr)))
    (project (find-project-by-name project-name)))
   (create-local-checkout project)
   (git-cmd project "branch"
    (forgerie-core:branch-name (forgerie-core:merge-request-source-branch mr))
    (forgerie-core:commit-sha (forgerie-core:branch-commit (forgerie-core:merge-request-source-branch mr))))
   (git-cmd project "branch"
    (forgerie-core:branch-name (forgerie-core:merge-request-target-branch mr))
    (forgerie-core:commit-sha (forgerie-core:branch-commit (forgerie-core:merge-request-source-branch mr))))
   (git-cmd project "checkout"
    (forgerie-core:branch-name (forgerie-core:merge-request-source-branch mr)))
   (mapcar
    (lambda (commit)
     (typecase commit
      (forgerie-core:commit (git-cmd project "merge" (forgerie-core:commit-sha commit)))
      (forgerie-core:patch
       (let
        ((patch-file (format nil "~A/working.patch" *working-directory*)))
        (with-open-file (str patch-file :direction :output :if-exists :supersede :if-does-not-exist :create)
         (princ (forgerie-core:patch-diff commit) str))
        (git-cmd project "am" patch-file)
        (delete-file patch-file)))))
    (forgerie-core:merge-request-changes mr))
   (git-cmd project "push" "gitlab" (forgerie-core:branch-name (forgerie-core:merge-request-source-branch mr)))
   (git-cmd project "push" "gitlab" (forgerie-core:branch-name (forgerie-core:merge-request-target-branch mr)))
   (let
    ((gl-mr
      (update-mapping (:merge-request (forgerie-core:merge-request-id mr))
       (post-request
        (format nil "projects/~A/merge_requests" (getf project :id))
        `(("source_branch" . ,(forgerie-core:branch-name (forgerie-core:merge-request-source-branch mr)))
          ("target_branch" . ,(forgerie-core:branch-name (forgerie-core:merge-request-target-branch mr)))
          ("title" . ,(forgerie-core:merge-request-title mr)))))))
    (mapcar
     (lambda (note) (create-note (getf gl-mr :project_id) "merge_requests" (getf gl-mr :id) note))
     (forgerie-core:merge-request-notes mr))))))

(defun create-snippet (snippet)
 (when-unmapped (:snippet (forgerie-core:snippet-id snippet))
  (when
   (/= 1 (length (forgerie-core:snippet-files snippet)))
   (error "Can only export snippets with exactly one file for now"))
  (handler-case
   (let*
    ((default-project (default-project))
     (file (first (forgerie-core:snippet-files snippet)))
     (gl-snippet
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
         ("file_name" . ,(forgerie-core:file-name file)))))))
    (list
     gl-snippet
     (mapcar
      (lambda (note) (create-note (getf default-project :id) "snippets" (getf gl-snippet :id) note))
      (forgerie-core:snippet-notes snippet))))
   (error (e) (format *error-output* "Failed to create snippet with title ~A, due to error ~A" (forgerie-core:snippet-title snippet) e)))))
