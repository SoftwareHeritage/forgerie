(in-package #:forgerie-gitlab)

(defmethod forgerie-core:system-postmortem ((system (eql :gitlab)))
 (format t "Doing gitlab's postmortem~%"))

(defmethod forgerie-core:display-mapping-error ((error (eql :linked-ticket-not-found)) object-id description)
 (format t "Ticket ~A not found, so can't link: ~A~%" object-id description))

(defmethod forgerie-core:display-mapping-error ((error (eql :gitlab-repository-has-no-projects)) object-id description)
 (format t "VC Repository ~A has no primary projects, so creating one.~%" object-id))

(defmethod forgerie-core:display-mapping-error ((error (eql :gitlab-ticket-assigned-to-default)) object-id description)
 (format t "~A~%" description))

(defmethod forgerie-core:display-mapping-error ((error (eql :gitlab-ticket-assigned-to-multiple)) object-id description)
 (format t "Ticket ~A assigned to multiple repositories, so can't assign~%" object-id))

(defmethod forgerie-core:display-mapping-error ((error (eql :gitlab-merge-request-not-assignable)) object-id description)
 (format t "Merge Request with id ~A can't be assigned to a repository~%" object-id))

(defmethod forgerie-core:display-mapping-error ((error (eql :user-avatar-too-big)) object-id description)
 (format t "~A~%" description))

(defmethod forgerie-core:display-mapping-error ((error (eql :gitlab-snippet-empty)) object-id description)
 (format t "~A~%" description))
