(in-package #:forgerie-core)

; This object will eventually have things like commits, for which comments
; can get attached to, as well as branches and tags and whatnot.
;
; Also the code location....  that seems important.
(defstruct vc-repository
 name

 ; Quite often, the slug will become the name
 slug

 ; Things like tickets in primary projects get added to the workspace
 ; in a single project per repository forget (like github)
 ;
 ; There's no restriction in core about uniqueness, or number of primary
 ; projects, and that's left open to the forgerie for a given forge to
 ; declare.
 primary-projects
 git-location
 projects

 ; repository access policy - one of :public, :internal, :private
 access-policy
 commits)
