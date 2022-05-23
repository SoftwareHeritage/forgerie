# Build

```
$ name=forgerie
$ docker build -t $name .
```

# Run script(s)

Edit /docker/config.lisp according to your runtime requirements, then run:
```
$ cd forgerie
$ docker run \
    -v $PWD/../forgerie:/opt/forgerie/ \
    -v $PWD/docker/ssh:/srv/forgerie/.ssh \
    -v /srv/phabricator:/srv/phabricator \
    -v ~/.kube:/srv/forgerie/.kube \
    -v /var/tmp/migrate-gitlab/forgerie:/tmp/forgerie \
    --name forgerie --net=host -it forgerie
```

# postmortem

After a crash, connect to the container with `docker exec` and execute
`/opt/forgerie/bin/postmortem`.

# ssh configuration

An ssh key pair is required for git commands to push repositories to migrate to the
gitlab instance. So first generate the keypair, then report such configuration in the
forgerie `config.lisp` (to make the migration script aware of it).

## Key generation

From your machine from the top-level of this repository:
```
ssh-keygen -t ed25519 -f ./docker/ssh/id_ed25519 -N ""
```

or from the container:
```
ssh-keygen -t ed25519 -f ~/.ssh/id_ed25519 -N ""
```


## config.lisp

We need the key to be loaded by the routine, so edit the `config.lisp` (copied out of
the template `config.lisp.tmpl`) with the following:

```
(with-open-file (file #P"/srv/forgerie/.ssh/id_ed25519.pub" :if-does-not-exist nil)
  (when file
    (setf forgerie-gitlab:*ssh-public-key* (read-line file nil nil))))
```
Note: `~` is `/srv/forgerie` in the docker context
