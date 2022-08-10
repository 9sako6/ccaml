FROM ubuntu:20.04

ENV LANG ja_JP.UTF-8
ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update \
  && yes | unminimize \
  && apt-get install -y \
  fonts-noto-cjk \
  gawk \
  git \
  man \
  opam \
  sudo \
  tzdata \
  vim \
  && apt-get -y clean \
  && rm -rf /var/lib/apt/lists/*

ENV TZ Asia/Tokyo

ARG USERNAME=9sako6
ARG GROUPNAME=user
ARG UID=9356
ARG GID=9356
ARG PASSWORD=user
RUN groupadd -g ${GID} ${GROUPNAME} \
  && useradd -m -s /bin/bash -u ${UID} -g ${GID} -G sudo ${USERNAME} \
  && echo ${USERNAME}:${PASSWORD} | chpasswd \
  && echo "$USERNAME ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

USER ${UID}

WORKDIR /workdir

ENV OCAML_VERSION=4.14.0

RUN opam init --disable-sandboxing \
  && eval $(opam env) \
  && opam switch create ${OCAML_VERSION} \
  && eval $(opam env) \
  && opam install --yes dune \
  utop \
  ocaml-lsp-server \
  ocamlformat \
  alcotest

RUN echo 'export PATH="~/.opam/${OCAML_VERSION}/bin:${PATH}"' >> ~/.bashrc \
  && echo 'eval $(opam env)' >> ~/.bashrc
