(in-package #:forgerie-core)

(defstruct merge-request-change
 ; A change can be a <commit> or a <patch> (from base)
 change
 ; A list of merge-request-change-comment, though there may be nested comments
 comments)

; For comments that are in the changes for the PRs
(defstruct merge-request-change-comment
 ; list of two items, the start and ending lines post change
 new-line
 ; list of two items, the start and ending lines pre change
 old-line
 text
 file
 author
 date
 ; Replies are of type merge-request-change-comment, though the line number
 ; doesn't matter
 replies)

; For actions that happened on a merge-request
(defstruct merge-request-action
 id
 author
 date
 type)

(defstruct merge-request
 id
 vc-repository
 title
 description
 date
 author

 ; The type can be one of:
 ; - :open
 ; - :closed
 type

 ; These branches may not currently exist, which is why we also need to
 ; know what commits were involved in the merge request.  That way we can
 ; recreate the data as it existed when the merge request happened.
 source-branch ; the base of the merge request
 target-branch ; the branch holding the changes for the merge

 ; Changes is a list of things to be applied.  Each is of the type merge-request-change
 changes

 ; Change comments here differ from change comments on the changes, as those
 ; are attached to commits, and they differ from the notes here, as they are
 ; comments that go on the files, with locations and replies, rather than general
 ; notes added to the merge request as a whole
 other-change-comments

 ; Actions.  Each is of the type merge-request-action
 actions

 ; All the comments
 notes)
