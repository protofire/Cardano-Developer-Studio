# Use an appropriate base image
FROM ubuntu:20.04

# Set environment variables
ENV USER=user
ENV HOME=/home/${USER}
ENV TEMPDir=/tmp
ENV BUILDARCH=x86_64
ENV IOKH_LIBSECP251_GIT_REV=ac83be33d0956faf6b7f61a60ab524ef7d6a473a
ENV IOHK_LIBSODIUM_GIT_REV=66f017f16633f2060db25e17c170c2afa0f2a8a1
ENV CABAL_VERSION=3.6.2.0
ENV GHC_VERSION=8.10.7
ENV HLS_VERSION=1.7.0.0

# Create the user
RUN useradd -ms /bin/bash ${USER}

# Install necessary packages
RUN apt-get update -y && \
    apt-get install -y \
    software-properties-common && \
    add-apt-repository ppa:rmescandon/yq -y && \
    apt-get update -y && \
    apt-get install -y \
    curl \
    xz-utils \
    automake \
    build-essential \
    g++ \
    git \
    jq \
    libicu-dev \
    libffi-dev \
    libgmp-dev \
    libncursesw5 \
    libpq-dev \
    libssl-dev \
    libsystemd-dev \
    libtinfo-dev \
    libtool \
    make \
    pkg-config \
    tmux \
    wget \
    zlib1g-dev \
    libreadline-dev \
    llvm \
    libnuma-dev \
    sudo \
    vim \
    apt-file \
    liblzma-dev \
    lsof \
    grep \
    coreutils \
    yq

# Clone and install secp256k1
RUN cd ${TEMPDir} && \
    git clone https://github.com/bitcoin-core/secp256k1 && \
    cd secp256k1 && \
    git fetch --all --tags && \
    git checkout ${IOKH_LIBSECP251_GIT_REV} && \
    ./autogen.sh && \
    ./configure --prefix=/usr --enable-module-schnorrsig --enable-experimental && \
    make && \
    make install && \
    cd .. && \
    rm -rf secp256k1

# Clone and install libsodium
RUN cd ${TEMPDir} && \
    git clone https://github.com/input-output-hk/libsodium.git && \
    cd libsodium && \
    git fetch --all --tags && \
    git checkout ${IOHK_LIBSODIUM_GIT_REV} && \
    ./autogen.sh && \
    ./configure --prefix=/usr && \
    make && \
    make install && \
    cd .. && \
    rm -rf libsodium

# Download ghcup
RUN wget --secure-protocol=TLSv1_2 https://downloads.haskell.org/~ghcup/${BUILDARCH}-linux-ghcup && \
    chmod +x ${BUILDARCH}-linux-ghcup && \
    mv ${BUILDARCH}-linux-ghcup ${HOME}/.ghcup/bin/ghcup

# Install GHC, Cabal, and HLS
RUN mkdir -p ${HOME}/.ghcup/bin && \
    chown -R ${USER}:${USER} ${HOME}/.ghcup && \
    sudo -u ${USER} -H bash -c " \
        export PATH=\$PATH:${HOME}/.ghcup/bin && \
        ghcup config set cache true && \
        ghcup install ghc ${GHC_VERSION} && \
        ghcup install cabal ${CABAL_VERSION} && \
        ghcup set ghc ${GHC_VERSION} && \
        ghcup install hls ${HLS_VERSION} && \
        ghcup config set cache false && \
        ghcup gc --cache \
    "

# Update cabal
RUN sudo -u ${USER} -H bash -c " \
    export PATH=\$PATH:${HOME}/.ghcup/bin:${HOME}/.cabal/bin && \
    cabal update \
"

# Download and install stylish-haskell
RUN wget https://github.com/rober-m/stylish-haskell/releases/download/v0.14.3.0/x86_64-linux-stylish-haskell && \
    chmod +x x86_64-linux-stylish-haskell && \
    mv x86_64-linux-stylish-haskell /usr/bin/stylish-haskell

# Set default user
USER ${USER}
WORKDIR ${HOME}
