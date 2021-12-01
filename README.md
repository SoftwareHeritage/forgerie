# Forgerie

Forgerie is an open source tool for converting software projects from
one collaborative development forge to another (e.g., Phabricator ->
GitLab).

See the [DESIGN.md](DESIGN.md) for design decisions.

# Running

Configure:

```
$ cp config/config.lisp.tmpl config/config.lisp 
$ $EDITOR config/config.lisp
```

Run `bin/run`
