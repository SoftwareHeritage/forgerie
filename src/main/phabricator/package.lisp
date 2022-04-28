(defpackage #:forgerie-phabricator (:use :cl)
 (:export
  #:*database-password* #:*revisions-to-skip* #:*phabricator-location*
  #:*project-assignment-overrides* #:*working-directory* #:*repository-overrides*
  #:*pastes-to-skip* #:*user-overrides* #:*storage-location* #:*confidential-space-phids*
  #:*included-repositories*))
