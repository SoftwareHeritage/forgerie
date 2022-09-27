# Forgerie

Forgerie is an open source tool for converting software projects from
one collaborative development forge to another (e.g., Phabricator ->
GitLab).

See the [DESIGN.md](DESIGN.md) for design decisions.

# Install quicklisp

Forgerie requires quicklisp on sbcl.  Generally you can get sbcl through your
distribution.  Once installed, you then need to install quicklsip.

Follow the installation instructions at [quicklisp.org](https://www.quicklisp.org/beta/)

# Running

## Configure

To configure:

```
$ cp config/config.lisp.tmpl config/config.lisp
$ $EDITOR config/config.lisp
```

A lot of these configuration options will lead you to need to take other actions,
such as making sure the database for phabricator is local to the machine.

You'll also need to ensure that keys are set up on various machines that will need
to be sshed to.

## Run the script

Run `bin/run`

# Postmortem

After run, if `forgerie-core:*log-mapping-errors*` is turned on, running
`bin/postmortem` will dump out a log of all the errors that happened
during the run

# Generating config.lisp.tmpl

There's a helper script `bin/build-config-tmpl` to generate `config/config.lisp.tmpl`
from inline documentation.
