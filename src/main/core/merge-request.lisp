(in-package #:forgerie-core)

(defstruct merge-request
 vc-repository
 title
 description

 ; The type can be one of:
 ; - :open
 ; - :closed
 type

 ; These branches may not currently exist, which is why we also need to
 ; know what commits were involved in the merge request.  That way we can
 ; recreate the data as it existed when the merge request happened.
 source-branch ; the base of the merge request
 target-branch ; the branch holding the changes for the merge

 ; Changes can be either a list of commits, or a list of patches (or both)
 ; which are applied after the branches are created
 ;
 ; These should be in the order they should be applied
 changes

 ; All the comments
 notes)
