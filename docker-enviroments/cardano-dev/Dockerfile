########################################################################################################################################################################

ARG UBUNTU_VERSION=20.04
###
ARG USER=plutus
ARG USER_GROUP=${USER}
ARG USER_UID=1000
ARG USER_GID=1000
###
ARG HOME=/home/${USER}
ARG WORK=${HOME}/workspace
###
ARG BUILDARCH=x86_64
# ARG BUILDARCH=arm64
###
ARG IOKH_LIBSECP251_GIT_REV=ac83be33d0956faf6b7f61a60ab524ef7d6a473a
ARG IOHK_LIBSODIUM_GIT_REV=66f017f16633f2060db25e17c170c2afa0f2a8a1
ARG CABAL_VERSION=3.6.2.0
ARG GHC_VERSION=8.10.7
ARG HLS_VERSION=1.7.0.0
###
ARG TEMPDir=/tmp
###

########################################################################################################################################################################

FROM --platform=linux/${BUILDARCH} ubuntu:${UBUNTU_VERSION} as system_deps
ARG TEMPDir
ENV DEBIAN_FRONTEND=nonintercative
WORKDIR ${TEMPDir}
# Allow ubuntu to cache package downloads
RUN rm -f /etc/apt/apt.conf.d/docker-clean; echo 'Binary::apt::APT::Keep-Downloaded-Packages "true";' > /etc/apt/apt.conf.d/keep-cache
# development dependencies
RUN --mount=type=cache,target=/var/cache/apt,sharing=locked,id=var.cache.apt \
    --mount=type=cache,target=/var/lib/apt,sharing=locked,id=var.lib.apt \
    apt-get update -y && apt-get install  -y \
    curl \
    xz-utils \
    automake \
    build-essential \
    g++\
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
    zlib1g-dev libreadline-dev llvm libnuma-dev \
    software-properties-common \
    sudo \
    vim \
    apt-file \ 
    liblzma-dev \
    lsof \
    grep \
    coreutils && \
    add-apt-repository ppa:rmescandon/yq -y && apt update && apt-get install -y \
    yq 

RUN sed -i '/Binary::apt::APT::Keep-Downloaded-Packages "true";/d' /etc/apt/apt.conf.d/keep-cache && \
    rm -rf /var/lib/apt/lists/*

########################################################################################################################################################################

FROM system_deps as system_deps_download_secp256k1
# system_deps args
ARG IOKH_LIBSECP251_GIT_REV
# install secp2561k library with prefix '/'
RUN git clone https://github.com/bitcoin-core/secp256k1 && \
    cd secp256k1 \
    && git fetch --all --tags && \
    git checkout ${IOKH_LIBSECP251_GIT_REV} 

####################################################################################

FROM system_deps as system_deps_download_libsodium
# system_deps args
ARG IOHK_LIBSODIUM_GIT_REV
# install libsodium from sources with prefix '/'
RUN git clone https://github.com/input-output-hk/libsodium.git && \
  cd libsodium \
  && git fetch --all --tags && \
  git checkout ${IOHK_LIBSODIUM_GIT_REV}

####################################################################################

FROM system_deps as system_deps_with_libsodium_and_secp256k1
ARG TEMPDir
# create a new image with libsodium and secp256k1, copy the libsodium and secp256k1 git clone dirs from the previous images
COPY --from=system_deps_download_secp256k1 ${TEMPDir}/secp256k1 secp256k1
RUN cd secp256k1 && \
    ./autogen.sh && \
    ./configure --prefix=/usr --enable-module-schnorrsig --enable-experimental && \
    make && \
    make install  && cd .. && rm -rf ./secp256k1

COPY --from=system_deps_download_libsodium ${TEMPDir}/libsodium libsodium
RUN cd libsodium && \
    ./autogen.sh && \
    ./configure --prefix=/usr && \
    make && \
    make install  && cd .. && rm -rf ./libsodium 

####################################################################################

FROM system_deps_with_libsodium_and_secp256k1 as system_deps_with_user
ARG USER
ARG USER_UID
ARG USER_GID
ARG HOME
ARG WORK
ENV USER=${USER}
ENV USER_UID=${USER_UID}
ENV USER_GID=${USER_GID}
ENV HOME=${HOME}
ENV WORK=${WORK}
RUN groupadd -g ${USER_GID} ${USER} && \ 
    useradd -u ${USER_UID} -g ${USER_GID}  -ms /bin/bash ${USER} && \ 
    echo "${USER} ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
RUN chown ${USER}:${USER} -hR ${TEMPDir}
USER ${USER}

####################################################################################

FROM system_deps as system_deps_download_ghcup
ARG BUILDARCH
RUN if [ "${BUILDARCH}" = "arm64" ]; then \
        wget --secure-protocol=TLSv1_2 https://downloads.haskell.org/~ghcup/aarch64-linux-ghcup && \
        chmod +x aarch64-linux-ghcup; \
        mv aarch64-linux-ghcup ${BUILDARCH}-linux-ghcup; \
    else \
        wget --secure-protocol=TLSv1_2 https://downloads.haskell.org/~ghcup/${BUILDARCH}-linux-ghcup && \
        chmod +x ${BUILDARCH}-linux-ghcup; \
    fi

####################################################################################

FROM system_deps as system_deps_download_stylish_haskell
ARG BUILDARCH 
# install stylish-haskell (FIXME: couldn't compile a bin for arm64)
# RUN wget https://github.com/rober-m/stylish-haskell/releases/download/v0.14.3.0/${BUILDARCH}-linux-stylish-haskell
# RUN chmod +x ${BUILDARCH}-linux-stylish-haskell
# RUN mv ${BUILDARCH}-linux-stylish-haskell /usr/bin/stylish-haskell
RUN wget https://github.com/rober-m/stylish-haskell/releases/download/v0.14.3.0/x86_64-linux-stylish-haskell  && \
    chmod +x x86_64-linux-stylish-haskell  

####################################################################################

FROM system_deps_with_user as system_deps_with_ghcup_and_cabal
ARG BUILDARCH 
ARG CABAL_VERSION
ARG GHC_VERSION
ARG HLS_VERSION
ARG TEMPDir

RUN mkdir -p ${HOME}/.ghcup/bin 
COPY --chown=${USER}:${USER} --from=system_deps_download_ghcup ${TEMPDir}/${BUILDARCH}-linux-ghcup ${HOME}/.ghcup/bin/ghcup 

ENV PATH=${PATH}:${HOME}/.ghcup/bin

# install ghc, caball, and hls
RUN --mount=type=cache,target=${HOME}/.ghcup/bin${TEMPDir},id=home.ghcup.tmp,uid=${USER_UID},gid=${USER_GID} \ 
    --mount=type=cache,target=${HOME}/.ghcup/cache,id=home.ghcup.cache,uid=${USER_UID},gid=${USER_GID} \ 
    sudo chown -hR ${USER}:${USER} ${HOME}/.ghcup && \
    # ghcup config set downloader Wget && \
    ghcup config set cache true && \
    ghcup install ghc ${GHC_VERSION} 

RUN --mount=type=cache,target=${HOME}/.ghcup/bin${TEMPDir},id=home.ghcup.tmp,uid=${USER_UID},gid=${USER_GID} \ 
    --mount=type=cache,target=${HOME}/.ghcup/cache,id=home.ghcup.cache,uid=${USER_UID},gid=${USER_GID} \ 
    ghcup install cabal ${CABAL_VERSION} 

RUN --mount=type=cache,target=${HOME}/.ghcup/bin${TEMPDir},id=home.ghcup.tmp,uid=${USER_UID},gid=${USER_GID} \ 
    --mount=type=cache,target=${HOME}/.ghcup/cache,id=home.ghcup.cache,uid=${USER_UID},gid=${USER_GID} \ 
    ghcup set ghc ${GHC_VERSION} && \
    ghcup install hls ${HLS_VERSION} 

RUN rm -fr ${HOME}/.ghcup/cache && \
    ghcup config set cache false && \
    ghcup gc --cache

# Add cabal to PATH
RUN echo "export PATH=$PATH:${HOME}/.cabal/bin" >> ${HOME}/.bashrc

# Update cabal
RUN --mount=type=cache,target=${HOME}/.cabal,sharing=locked,id=home.cabal,uid=${USER_UID},gid=${USER_GID} \ 
    sudo chown -hR ${USER}:${USER} ${HOME}/.cabal && \
    cabal update 

# RUN --mount=type=cache,target=${HOME}/.cabal,sharing=locked,id=home.cabal,uid=${USER_UID},gid=${USER_GID} \ 
#     cabal install implicit-hie --overwrite-policy=always -v

RUN --mount=type=cache,target=${HOME}/.cabal,sharing=locked,id=home.cabal,uid=${USER_UID},gid=${USER_GID} \ 
    cp -r --archive ${HOME}/.cabal ${TEMPDir}/nix.cabal 

RUN sudo rm -fr ${HOME}/.cabal && \
    sudo mv ${TEMPDir}/nix.cabal ${HOME}/.cabal
    
COPY --chown=${USER}:${USER} --from=system_deps_download_stylish_haskell ${TEMPDir}/x86_64-linux-stylish-haskell /usr/bin/stylish-haskell

####################################################################################

# FROM system_deps_with_ghcup_and_cabal as system_deps_download_toolkit
# ARG TOOLKIT_VERSION_VERSION_GIT="tags/v1.1"
# RUN git clone  https://github.com/manupadillaph/cardano-tools-scripts.git && \
#     cd cardano-tools-scripts  && \
#     git checkout ${TOOLKIT_VERSION_VERSION_GIT} 

####################################################################################

# FROM system_deps_with_ghcup_and_cabal as system_deps_with_tools
# ARG TEMPDir
# COPY --chown=${USER}:${USER} --from=system_deps_download_toolkit ${TEMPDir}/cardano-tools-scripts ${TOOLS}/cardano-tools-scripts

# RUN echo "source ${SCRIPTS}/env/tools-env-init.sh" >> ${HOME}/.bashrc && \
#     echo "export NIXX=${SCRIPTS}/env/direnv-switch.sh" >> ${HOME}/.bashrc && \
#     echo "export NIX=${SCRIPTS}/env/activate-shell-nix.sh" >> ${HOME}/.bashrc

# USER root
# RUN chown ${USER}:${USER} -hR "${TOOLS}" && \
#     find "${TOOLS}" -type d -exec chmod 755 {} \; && \
#     find "${TOOLS}" -type f -exec chmod u+w  {} \; && \
#     find "${TOOLS}" -name "*.sh" -type f -exec chmod +x {} \;

# USER ${USER}
# RUN sudo rm -rf ${TEMPDir}/*
# WORKDIR ${WORK}

####################################################################################

FROM system_deps_with_ghcup_and_cabal as system_ready
ARG TEMPDir
USER ${USER}
RUN sudo rm -rf ${TEMPDir}/*
WORKDIR ${WORK}

########################################################################################################################################################################