(in-package #:forgerie-gitlab)

(defvar *server-address* nil
 "The fully qualitifed server address for the gitlab instance, including the scheme to
 use, e.g. https://gitlab.yourdomain.tld")

(defvar *private-token* nil
 "The private token with which to access the gitlab instance.  Needs to be
 set up either from within gitlab, or via a script that uses the rails console
 directory")

(defvar *default-project* nil
 "A plist of the form '(:name NAME :slug SLUG :disable-tickets DISABLE)
 for the default project in which things like snippets, tickets that can't
 be assigned to a project, and other misc items go.  NAME is the proper name
 of the project, with SLUG being the url slug to access it.  If DISABLE is
 set to T, then tickets that can't be assigned to a project will not be
 assigned to this default project.")

(defvar *ssh-public-key* nil
 "The public key that should be installed for the running user so that git commands
 work correctly.")

(defvar *default-group* nil
 "A plist of the form '(:name NAME :path SLUG) that defines the group in which
 all created projects will be placed.  NAME is the proper name for the group,
 while SLUG is the url slug.  If NIL, the projects will all be created at the
 top level.")

(defvar *group-structure* nil
 "A list of plists of the form '(:name NAME :path SLUG :parent SLUG) that
 define the base structure of groups configured in the gitlab instance")

; For development only.  Will limit all exporting to things having
; to do with the project with the name provided.
(defvar *single-project* nil)

; The default rails-command to execute the rails console.
(defvar *rails-command* "/usr/bin/ssh"
  "The rails command to execute, by default this uses ssh. But one could use kubectl or
docker instead.")

; The required args for the *rails-command*.
(defvar *rails-command-args* nil
 "By default, using ssh, a tuple of the form '(HOST COMMAND) that informs the gitlab
 forgerie how to run rails commands over ssh. It will always use SSH, even if set up to
 run on localhost, so keys must be installed to ssh to localhost. When overriden to
 another command like kubectl, a list '(COMMAND) for the necessary extra args that the
 command requires to run.

 When using ssh, an example for a server using docker might be:

 '(\"ssh.gitlab.yourdomain.tld\" \"docker exec -i gitlab /opt/gitlab/bin/gitlab-rails c\")

 A useful thing to do is to run ssh on the server for non git purposes on port 2222, and
 then set up your .ssh/config to have the following:

 Host ssh.gitlab.yourdomain.tld
   User <user>
   Port 2222
   IdentityFile ~/.ssh/your_identity_file

 When using kube, an example might be:

 '(\"exec -ti -n gitlab-system deployment/gitlab-toolbox -- /srv/gitlab/bin/rails console\")

")

(defvar *merge-request-suffix* nil
 "A function that takes an argument of a forgerie-core:merge-request and
 returns a string that will be appended to the description of created merge requests.

 Useful to create backlinks to the previous system, or addition migration information")

(defvar *ticket-suffix* nil
 "A function that takes an argument of a forgerie-core:ticket and
 returns a string that will be appended to the description of created tickets (issues).

 Useful to create backlinks to the previous system, or addition migration information")

(defvar *snippet-suffix* nil
 "A function that takes an argument of a forgerie-core:snippet and
 returns a string that will be appended to the description of created snippets (pastes).

 Useful to create backlinks to the previous system, or addition migration information")

(defvar *fallback-file-text* nil
 "A function that takes a forgerie-core:file, and returns the (markdown) text that will
 be output when the file was too large to be uploaded.

 Useful to create a link to the previous system on files that are not migrated yet")

(defvar *fallback-mapped-item-text* nil
 "A function that takes an argument of a mapped item reference (as can be found in notes),
 and returns the (markdown) text that will be output when the mapped item is not found.

 Useful to create a link to the previous system on references that are not migrated yet")

(defvar *limit-to-active-users* nil
 "If non nil, will only add users to the gitlab instance if they are active in the
 items also coming over for processing.  Useful when doing piecemeal conversions.")

(defvar *always-migrate-users* nil
 "A list of usernames of users that should always be migrated, even when only migrating
 active users. Useful to give all of your known contributors an account at once, even on
 a partial migration.")
