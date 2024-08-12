# Plutus Contract Examples

This directory `./examples` contains a collection of examples for Plutus validator contracts and minting policies, along with their corresponding tests. Each contract example is organized into its own directory with the following structure:

- **`src/`**: Contains the Haskell source code for the validator contract or minting policy.
- **`test/`**: Contains the Haskell source code for the tests related to the contract or policy.
- **`.cabal`**: The Cabal configuration file for building and testing the project.

## Set Up Cabal -  Haskell Enviroment

### Set environment variables

```
export USER=$(whoami)
export HOME=/home/${USER}
export TEMPDir=/tmp

#x86_64 | arm64
export BUILDARCH=x86_64
export IOKH_LIBSECP251_GIT_REV=ac83be33d0956faf6b7f61a60ab524ef7d6a473a
export IOHK_LIBSODIUM_GIT_REV=66f017f16633f2060db25e17c170c2afa0f2a8a1
export CABAL_VERSION=3.6.2.0
export GHC_VERSION=8.10.7
export HLS_VERSION=1.7.0.0
```

### Install necessary packages

```
sudo add-apt-repository ppa:rmescandon/yq -y
sudo apt-get update -y
sudo apt-get install -y \
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
    software-properties-common \
    sudo \
    vim \
    apt-file \
    liblzma-dev \
    lsof \
    grep \
    coreutils \
    yq
```

### Download in Temp fodler

```
cd $TEMPDir
```

### Clone and install secp256k1

```
git clone https://github.com/bitcoin-core/secp256k1
cd secp256k1
git fetch --all --tags
git checkout ${IOKH_LIBSECP251_GIT_REV}
./autogen.sh
./configure --prefix=/usr --enable-module-schnorrsig --enable-experimental
make
sudo make install
cd ..
rm -rf secp256k1

```

### Clone and install libsodium

```
git clone https://github.com/input-output-hk/libsodium.git
cd libsodium
git fetch --all --tags
git checkout ${IOHK_LIBSODIUM_GIT_REV}
./autogen.sh
./configure --prefix=/usr
make
sudo make install
cd ..
rm -rf libsodium
```

### Download ghcup

```
wget --secure-protocol=TLSv1_2 https://downloads.haskell.org/~ghcup/${BUILDARCH}-linux-ghcup
chmod +x ${BUILDARCH}-linux-ghcup
mv ${BUILDARCH}-linux-ghcup ${HOME}/.ghcup/bin/ghcup
```

### Install GHC, Cabal, and HLS

```
sudo -u ${USER} -H bash -c "
    mkdir -p ${HOME}/.ghcup/bin
    export PATH=${PATH}:${HOME}/.ghcup/bin
    ghcup config set cache true
    ghcup install ghc ${GHC_VERSION}
    ghcup install cabal ${CABL_VERSION}
    ghcup set ghc ${GHC_VERSION}
    ghcup install hls ${HLS_VERSION}
    ghcup config set cache false
    ghcup gc --cache
"
```

### Update cabal

```
sudo -u ${USER} -H bash -c "
    export PATH=${PATH}:${HOME}/.ghcup/bin:${HOME}/.cabal/bin
    cabal update
"
```

### Download and install stylish-haskell

```
wget https://github.com/rober-m/stylish-haskell/releases/download/v0.14.3.0/x86_64-linux-stylish-haskell
chmod +x x86_64-linux-stylish-haskell
sudo mv x86_64-linux-stylish-haskell /usr/bin/stylish-haskell
```

# TODO:

FROM system_deps_with_ghcup_and_cabal as system_with_docker
USER root
RUN curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
RUN add-apt-repository \
    "deb [arch=amd64] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable"
RUN apt-get update
RUN apt-get install -y docker-ce docker-ce-cli containerd.io
# Add user to the docker group
ARG DOCKER_GID
RUN groupmod -g ${DOCKER_GID} docker || groupadd -g ${DOCKER_GID} docker && usermod -aG docker ${USER}
USER ${USER}
RUN newgrp docker

## Getting Started

To use each contract example, follow these steps:

1. **Build the project**:
   ```bash
   cabal build
   ```

2. **Run the tests**:
   ```bash
   cabal test
   ```

If you want to build and test all the examples in the `examples` directory, you can use the following commands:

- **Build all examples**:
  ```bash
  cabal build all
  ```

- **Run all tests**:
  ```bash
  cabal test all
  ```

## Directory Structure

Each example is organized into its own directory within the `examples` directory. The structure of each example directory is:

- `src/` - Contains the main Haskell source files for the example.
- `test/` - Contains the Haskell source files for the tests.
- `.cabal` - The Cabal configuration file.

## Examples

For detailed information about each example, navigate to the specific example directory and review the `README.md` files and source code there.

## Requirements

Make sure you have the following installed to build and test the projects:

- [GHC 8.10.7](https://www.haskell.org/ghc/)
- [Cabal 3.10.3](https://www.haskell.org/cabal/)

If you encounter any issues or need further assistance, please refer to the Plutus documentation or seek help from the Plutus community.
