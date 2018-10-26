FROM centos:centos7

WORKDIR /tmp

RUN yum -y install epel-release

RUN curl -O http://li.nux.ro/download/nux/dextop/el7/x86_64/nux-dextop-release-0-5.el7.nux.noarch.rpm
RUN rpm -Uvh nux-dextop-release*rpm

RUN yum -y install \
        automake \
        autoconf \
        bison \
        doxygen \
        flex \
        git \
        gcc-c++ \
        graphviz \
        java-1.8.0-openjdk-devel \
        libtool \
        libsqlite3x-devel \
        make \
        mcpp \
        ncurses-devel \
        zlib-devel

RUN useradd --create-home --shell /bin/bash souffle

USER souffle

WORKDIR /home/souffle
