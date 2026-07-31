FROM mcr.microsoft.com/devcontainers/base:ubuntu-24.04

ARG IDRIS2_VERSION=0.7.0

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        build-essential \
        ca-certificates \
        chezscheme \
        git \
        libffi-dev \
        libgmp-dev \
        libncurses-dev \
        libssl-dev \
        make \
        pkg-config \
    && rm -rf /var/lib/apt/lists/*

RUN git clone --depth 1 --branch v${IDRIS2_VERSION} https://github.com/idris-lang/Idris2.git /tmp/Idris2 \
    && make -C /tmp/Idris2 bootstrap SCHEME=chezscheme \
    && make -C /tmp/Idris2 install \
    && rm -rf /tmp/Idris2
