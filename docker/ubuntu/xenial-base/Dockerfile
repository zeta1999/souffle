FROM ubuntu:xenial

WORKDIR /tmp

RUN apt-get update -y
RUN apt-get install -y \
        autoconf \
        automake \
        bison \
        build-essential \
        clang \
        clang-format-4.0 \
        doxygen \
        flex \
        g++ \
        git \
        graphviz \
        libsqlite3-dev \
        libtool \
        lsb-release \
        libncurses5-dev \
        make \
        mcpp \
        python \
        sqlite \
        zlib1g-dev

RUN useradd --create-home --shell /bin/bash souffle

USER souffle

WORKDIR /home/souffle
