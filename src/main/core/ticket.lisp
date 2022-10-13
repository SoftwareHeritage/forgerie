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

 ; ticket-action objects
 actions

 linked-tickets
 subscribers)

(defstruct ticket-action
 id
 author
 date

 ; can be a change of:
 ; - :open -> opening ticket (newvalue = new state)
 ; - :close -> closing ticket (newvalue = close reason)
 ; - :title
 ; - :description (newvalue = list of parsed comment entries)
 ; - :priority
 ; - :status
 ; - :assignee (newvalue = user)
 ; - :subscribers (newvalue = list of users)
 ; - :mergedinto (newvalue = shallow ticket struct)
type

 newvalue)
