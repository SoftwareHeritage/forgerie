This is just a dumping ground about design of project.  This should get updated
at some point into a nice form, but for now it's just a list of things as I
think of them.

* Imports come from database.  Don't use api to import information, except as reference.
  For instance, if the username from the api is different than in the database, figure
  out why and adjust.
* Export go to api first, and then follow up with database edits.  The API for any given
  system should set up objects correctly.  Then, after initial, edit database directly.
  For instance, the user created date is probably set to `time.now()` when creating through
  the api, but we may want the user creation date to be set to what it was in the previous
  system.
* Each forge gets it's own component, and implements some top level generic methods.
