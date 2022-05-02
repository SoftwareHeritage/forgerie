(in-package #:forgerie-gitlab)

(defvar *server-address* nil
 "The fully qualitifed server address for the gitlab instance, including the scheme to
 use, e.g. https://gitlab.yourdomain.tld")

(defvar *private-token* nil
 "The private token with which to access the gitlab instance.  Needs to be
 set up either from within gitlab, or via a script that uses the rails console
 directory")

(defvar *working-directory* (format nil "~Agitlab/" forgerie-core:*working-directory*))

(defvar *default-project* nil
 "A plist of the form '(:name NAME :slug SLUG) for the default project
 in which things like snippets, tickets that can't be assigned to a project,
 and other misc items go.  NAME is the proper name of the project, with
 SLUG being the url slug to access it.")

(defvar *ssh-public-key* nil
 "The public key that should be installed for the running user so that git commands
 work correctly.")

(defvar *default-group* nil
 "A plist of the form '(:name NAME :path SLUG) that defines the group in which
 all created projects will be placed.  NAME is the proper name for the group,
 while SLUG is the url slug.  If NIL, the projects will all be created at the
 top level.")

; For development only.  Will limit all exporting to things having
; to do with the project with the name provided.
(defvar *single-project* nil)

; The args (host and command are normal) for the ssh command to
; boot the rails console.  Sometimes this is localhost.  Keys
; have to be set up.
(defvar *rails-console-ssh-args* nil
 "A tuple of the form '(HOST COMMAND) that informs the gitlab forgerie
 how to run rails commands over ssh.  It will always use SSH, even if
 set up to run on localhost, so keys must be installed to ssh to localhost.

 An example for a server using docker might be:

 '(\"ssh.gitlab.yourdomain.tld\" \"docker exec -i gitlab /opt/gitlab/bin/gitlab-rails c\")

 A useful thing to do is to run ssh on the server for non git purposes on port 2222, and then
 set up your .ssh/config to have the following:

 Host ssh.gitlab.yourdomain.tld
   User <user>
   Port 2222
   IdentityFile ~/.ssh/your_identity_file")

(defvar *merge-request-suffix* nil
 "A function that takes an argument of a forgerie-core:merge-request and
 returns a string that will be appended to the description of created merge requests.

 Useful to create backlinks to the previous system, or addition migration information")

(defvar *ticket-suffix* nil
 "A function that takes an argument of a forgerie-core:ticket and
 returns a string that will be appended to the description of created tickets (issues).

 Useful to create backlinks to the previous system, or addition migration information")

(defvar *limit-to-active-users* nil
 "If non nil, will only add users to the gitlab instance if they are active in the
 items also coming over for processing.  Useful when doing piecemeal conversions.")
