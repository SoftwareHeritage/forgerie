FROM debian:11
RUN export DEBIAN_FRONTEND=noninteractive && \
  apt-get update && apt-get upgrade -y && \
  apt-get install -y \
    cl-quicklisp \
    default-libmysqlclient-dev \
    git \
    kubernetes-client

RUN mkdir -p /srv/phabricator /opt/forgerie
RUN useradd -md /srv/forgerie -s /bin/bash forgerie
COPY docker/.sbclrc /srv/forgerie/.sbclrc
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
