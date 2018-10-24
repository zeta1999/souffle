FROM oraclelinux:7.3

WORKDIR /tmp

RUN yum install -y \
        autoconf \
        automake \
        bison \
        clang \
        doxygen \
        flex \
        gcc-c++ \
        git \
        graphviz \
        kernel-devel \
        ncurses-devel \
        sqlite-devel \
        libtool \
        make \
        mcpp \
        pkg-config \
        python \
        sqlite \
        zlib-devel

RUN yum install -y http://dl.fedoraproject.org/pub/fedora/linux/releases/27/Everything/x86_64/os/Packages/l/libmcpp-2.7.2-19.fc27.x86_64.rpm
RUN yum install -y http://dl.fedoraproject.org/pub/fedora/linux/releases/27/Everything/x86_64/os/Packages/m/mcpp-2.7.2-19.fc27.x86_64.rpm

RUN useradd --create-home --shell /bin/bash souffle

USER souffle

WORKDIR /home/souffle
