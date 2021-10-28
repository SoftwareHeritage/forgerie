(in-package #:forgerie-core)

(defstruct ticket
 id
 title
 author
 description
 projects

 ; The type can be one of:
 ; - :open
 ; - :closed
 type

 date
 notes
 priority)
