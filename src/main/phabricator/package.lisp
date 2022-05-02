(defpackage #:forgerie-phabricator (:use :cl)
 (:export
  #:*database-password* #:*database-username* #:*revisions-to-skip* #:*phabricator-location*
  #:*project-assignment-overrides* #:*repository-overrides*
  #:*pastes-to-skip* #:*user-overrides* #:*storage-location* #:*confidential-space-phids*
  #:*included-repositories* #:*staging-repository* #:*email-address-sanitizer*))
