FROM ubuntu:bionic

WORKDIR /tmp

RUN apt-get update -y && \
        apt-get upgrade -y && \
        apt-get install -y \
        curl \
        software-properties-common

RUN apt-add-repository ppa:ubuntu-toolchain-r/test

RUN curl -fsSL http://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
RUN apt-add-repository "deb http://apt.llvm.org/trusty/ llvm-toolchain-trusty-4.0 main"

RUN apt-get update -y && \
        apt-get upgrade -y && \
        apt-get install -y \
        automake \
        autoconf \
        bison \
        build-essential \
        clang-4.0 \
        doxygen \
        flex \
        g++-7 \
        git \
        graphviz \
        libncurses5-dev \
        libopenmpi-dev \
        libsqlite3-dev \
        libstdc++-7-dev \
        libtool \
        lsb-release \
        mcpp \
        openmpi-bin \
        zlib1g-dev

RUN useradd --create-home --shell /bin/bash souffle

USER souffle

WORKDIR /home/souffle
