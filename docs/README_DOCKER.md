Our scripts require Docker to run successfully, as it plays a critical role in creating isolated and reproducible environments for development and testing.

### What is Docker?

Docker is a platform that uses containerization technology to package an application and its dependencies into a container—a standardized executable component combining application source code with the operating system (OS) libraries and dependencies required to run that code in any environment.

Containers are isolated from one another and the host system, yet can communicate with each other through well-defined channels. Unlike virtual machines, containers do not bundle a full operating system—only libraries and settings required to make the software work are needed. This makes for efficient, lightweight, self-contained systems and guarantees that software will always run the same, regardless of where it’s deployed.

### Why Docker for Our Scripts?

Using Docker, we can easily create separate containers for each version of the Cardano node and network, as well as for the Cardano wallet and Cardano DB Sync. This allows us to:

- Ensure Consistency: Each container runs the same regardless of the host environment, from development to production.
- Simplify Configuration: Docker containers can be configured and started with a single command, without the need for complex setup procedures.
- Manage Dependencies: Each container encapsulates its own dependencies, preventing conflicts between projects or versions.
- Isolate Environments: By running different containers for different components (nodes, wallets, DB Sync), we prevent interference and allow for simultaneous, side-by-side operation of multiple configurations.

### Getting Started with Docker

Before proceeding with our scripts, you'll need to install Docker on your machine. Installation instructions vary depending on your operating system:

**Docker Desktop for Windows:**

- Visit the Docker Hub at https://www.docker.com/products/docker-desktop and download the installer for Windows.
- Run the installer and follow the instructions to install Docker Desktop on Windows.
- After installation, Docker will start automatically. You might need to log out and log back in or reboot your computer to complete the installation.

**Docker Desktop for Mac:**

- Visit the Docker Hub at https://www.docker.com/products/docker-desktop and download the installer for Mac.
- Open the `.dmg` file and drag Docker to the Applications folder.
- Run Docker from the Applications folder. Docker will request your password to install a helper tool.
- After installation, Docker will start automatically.

**Install using the repository:**

Update your package index and install packages to allow `apt` to use a repository over HTTPS:
```
sudo apt-get update
sudo apt-get install \
  ca-certificates \
  curl \
  gnupg \
  lsb-release
```

Add Docker’s official GPG key:
```
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg
```

Use the following command to set up the stable repository:
```
echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
```

Update `apt` package index, and install the latest version of Docker Engine and containerd:
```
sudo apt-get update
sudo apt-get install docker-ce docker-ce-cli containerd.io
```

Verify that Docker Engine is installed correctly by running the hello-world image:
```
sudo docker run hello-world
```

### Note to Users

The installation steps provided above are a general guide. Due to the rapid development of Docker and the differences between system configurations, it's recommended to refer to the official Docker documentation for the most accurate and detailed instructions:

- Docker Documentation: https://docs.docker.com/get-docker/

This ensures that users follow the latest guidelines and troubleshooting steps directly from Docker, accommodating any recent changes or system-specific requirements.