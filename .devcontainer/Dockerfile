FROM mcr.microsoft.com/devcontainers/base:ubuntu-24.04

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        build-essential \
        ca-certificates \
        chezscheme \
        clang \
        curl \
        gdb \
        git \
        git-lfs \
        jq \
        libffi-dev \
        libgmp-dev \
        libncurses-dev \
        libssl-dev \
        lsof \
        make \
        nodejs \
        npm \
        pkg-config \
        ripgrep \
        rlwrap \
        shellcheck \
        time \
        unzip \
        valgrind \
        wget \
        zip \
    && rm -rf /var/lib/apt/lists/*

ENV PATH="/home/vscode/.local/bin:${PATH}"

COPY .devcontainer/install-idris-toolchain.sh /tmp/install-idris-toolchain.sh
RUN bash /tmp/install-idris-toolchain.sh \
    && rm /tmp/install-idris-toolchain.sh
