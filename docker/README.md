# Build

```
$ name=forgerie
$ docker build -t $name .
```

# Run script(s)

Edit /docker/config.lisp according to your runtime requirements, then run:
```
$ docker run -it $name \
  -v /path/to/clone/forgerie/docker:/opt/forgerie/config \
  $script
```

for script in /opt/forgerie/bin/{build-config-tmpl, run, postmortem}


