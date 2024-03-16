# Welcome to Cardano Developer Studio

This repository is the home of the Cardano Developer Studio, an all-in-one suite designed to streamline the development process for Cardano blockchain developers. Our goal is to provide an accessible, comprehensive set of tools and resources to support developers in building dApps and smart contracts efficiently on the Cardano platform.

## Features

- Dockerfiles for Cardano Node, Cardano Wallet, cardano-cli, and cardano-db-sync
- Docker Compose configurations for easy setup and integration

## Getting Started

Detailed instructions on setting up your development environment and using the tools provided will be added to this README.

## Installing Docker

### Windows

**Docker Desktop for Windows:**

- Visit the Docker Hub at https://www.docker.com/products/docker-desktop and download the installer for Windows.
- Run the installer and follow the instructions to install Docker Desktop on Windows.
- After installation, Docker will start automatically. You might need to log out and log back in or reboot your computer to complete the installation.

### Mac

**Docker Desktop for Mac:**

- Visit the Docker Hub at https://www.docker.com/products/docker-desktop and download the installer for Mac.
- Open the `.dmg` file and drag Docker to the Applications folder.
- Run Docker from the Applications folder. Docker will request your password to install a helper tool.
- After installation, Docker will start automatically.

### Ubuntu

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

## Running Docker Compose Workflows

To streamline the setup and execution of the Cardano Developer Studio tools, we provide a utility script, `run.sh`, located in the `scripts` directory. This script simplifies the management of Docker Compose workflows, allowing for an intuitive selection of different configurations for development purposes.

### How to Use `run.sh`

1. **Open a terminal** and navigate to the root directory of the Cardano Developer Studio project.
2. **Execute the script** by running the following command:

```
bash scripts/run.sh
```

3. **Follow the on-screen prompts**. The main menu will provide you with the following options:
- `1) Docker Compose Workflow`: Initiates the Docker Compose workflow, allowing you to select which components to run (e.g., Cardano Node, Cardano Wallet).
- `2) Other Tool [Placeholder]`: Reserved for future tools and utilities.
- `3) Exit`: Exits the script.

### Docker Compose Menu

After selecting the Docker Compose Workflow, you will be presented with another menu to choose the specific component you wish to run:
- `1) Base`: Sets up the basic Docker environment, including necessary networks and volumes.
- `2) Cardano Node`: Starts the Cardano Node container with the configured environment.
- `3) Cardano Wallet`: Initiates the Cardano Wallet container setup.
- `4) Cardano DB Sync`: Begins synchronization with the Cardano blockchain database.
- `5) Exit`: Returns to the main menu.

For each selection, you will be prompted to enter environment variables such as `CARDANO_NODE_VERSION`, `CARDANO_NETWORK`, `CARDANO_NODE_DB_PATH`, and `CARDANO_NODE_PORT`. Default values are provided, but you may customize them as needed.

### Note:

- Before running the `run.sh` script, ensure Docker is installed and running on your system. Follow the installation instructions provided in the previous sections for your respective operating system.
- The `compose.sh` script, invoked by `run.sh`, handles the intricacies of configuring and starting the Docker Compose services based on your selections. It automatically sets necessary environment variables and permissions to ensure a smooth setup experience.


## Contribution

Contributions to the Cardano Developer Studio are welcome. Whether you're looking to fix bugs, add new features, or improve documentation, your help is appreciated. Please see our contribution guidelines for more information.

## License

This project is licensed under the GNU General Public License v3.0 - see the [LICENSE](LICENSE) file for details.


## Acknowledgements

We would like to thank the Cardano community for their support and contributions to this project.
