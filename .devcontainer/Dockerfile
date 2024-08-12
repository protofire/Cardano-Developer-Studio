# Use Ubuntu as the base image
FROM ubuntu:latest

# Avoid warnings by switching to noninteractive
ENV DEBIAN_FRONTEND=noninteractive

# Update package lists and cache this layer
RUN apt-get update

# Install essential tools
RUN apt-get install -y \
    apt-transport-https \
    ca-certificates \
    curl \
    gnupg-agent \
    software-properties-common \
    jq \
    lz4 \
    grep \
    sed \
    gawk \
    coreutils \
    sudo \
    && curl -fsSL https://download.docker.com/linux/ubuntu/gpg | apt-key add - \
    && add-apt-repository \
       "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
       $(lsb_release -cs) \
       stable"

# Install docker-ce-cli separately to ensure all previous commands are cached if docker-ce-cli version remains the same
RUN apt-get install -y docker-ce docker-ce-cli containerd.io
# Add user to the docker group
ARG DOCKER_GID=1001
RUN groupmod -g ${DOCKER_GID} docker || groupadd -g ${DOCKER_GID} docker && usermod -aG docker root
RUN newgrp docker
# Install rsync
RUN apt-get install -y rsync

# Install git
RUN apt-get install -y git
 
# Cleanup
RUN apt-get autoremove -y \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* /tmp/library-scripts

# Give non-root user sudo privileges (Optional)
# Here you can add a non-root user and give them sudo access.
# RUN useradd -m devstudio && echo "devstudio:devstudio" | chpasswd && adduser devstudio sudo

# Example usage: Allowing passwordless sudo operations for a specific user
# RUN echo 'devstudio ALL=(ALL) NOPASSWD: ALL' >> /etc/sudoers

# Switch back to dialog front-end for apt-get, if necessary
ENV DEBIAN_FRONTEND=dialog

# Start as root user
USER root
