FROM debian:11
RUN export DEBIAN_FRONTEND=noninteractive && \
  apt-get update && apt-get upgrade -y && \
  apt-get install -y \
    cl-quicklisp \
    default-libmysqlclient-dev \
    git \
    kubernetes-client \
    file

RUN useradd -md /srv/forgerie -s /bin/bash forgerie
RUN mkdir -p /srv/phabricator /opt/forgerie /srv/forgerie/bin
COPY docker/.sbclrc /srv/forgerie/.sbclrc
COPY docker/.gitconfig /srv/forgerie/.gitconfig
COPY docker/entrypoint.sh /srv/forgerie/bin/entrypoint.sh
COPY docker/ssh /srv/forgerie/.ssh
RUN chown -R forgerie:forgerie /srv/forgerie /opt/forgerie

USER forgerie
WORKDIR /srv/forgerie

# install quicklisp and pulls core dependencies for it to run properly
RUN sbcl --no-sysinit --no-userinit --noprint \
    --load /usr/share/common-lisp/source/quicklisp/quicklisp.lisp \
    --eval '(quicklisp-quickstart:install :path "quicklisp")' \
    --quit  # to install and configure quicklisp

COPY . /opt/forgerie
ENV FORGERIE_PATH=/opt/forgerie/

# install quicklisp and pulls core dependencies for it to run properly
RUN sbcl --quit

ENTRYPOINT [ "/srv/forgerie/bin/entrypoint.sh" ]
