(in-package #:forgerie-core)

(defstruct ticket
 id
 title
 author
 assignee
 description
 projects
 confidential

 ; The type can be one of:
 ; - :open
 ; - :closed
 type

 date
 notes
 priority

 linked-tickets)
