### Table of Contents
- [Table of Contents](#table-of-contents)
- [Introduction](#introduction)
- [Why Choose the DevContainer Method?](#why-choose-the-devcontainer-method)
- [Updating Bash and Installing Package Managers](#updating-bash-and-installing-package-managers)
  - [In Windows](#in-windows)
  - [In Mac](#in-mac)
  - [In Ubuntu (and other Linux Distributions)](#in-ubuntu-and-other-linux-distributions)
- [Note on Package Managers](#note-on-package-managers)
- [Running Bash Scripts](#running-bash-scripts)
  - [On Windows](#on-windows)
  - [On Mac](#on-mac)

### Introduction

Traditionally, setting up a development environment for a project involves several steps: installing dependencies, configuring software, and ensuring that all developers are working within a consistent environment. This process can be time-consuming and prone to variations across different machines.

We've streamlined this process by introducing a `Development Container for Visual Studio Code` users. If you're using VS Code as your IDE, you can now avoid the manual setup of your development environment by utilizing our DevContainer configuration. This method ensures that you're instantly ready to start developing, with all necessary tools and dependencies preconfigured in an isolated Docker container.

For those interested in taking advantage of this option, please refer to the section on Using the Development Container in VS Code for detailed instructions on how to get started. [Using the Development Container in VS Code](#using-the-development-container-in-vs-code)

This DevContainer approach is entirely optional. If you prefer to set up your environment manually or are not using VS Code, the following sections will guide you through the traditional setup process. However, we highly recommend trying out the DevContainer for a seamless development experience.

### Why Choose the DevContainer Method?

- Simplicity: Launch your development environment with a few clicks, without manual installations.
- Consistency: Every team member uses an identical setup, minimizing "works on my machine" issues.
- Portability: Your development environment can be replicated on any machine with Docker and VS Code, regardless of the host OS.

### Updating Bash and Installing Package Managers

#### In Windows

For Windows users, Bash can be accessed through Git Bash or WSL (Windows Subsystem for Linux). 

To update Bash within WSL:

Open your Linux distribution through WSL.
Run `sudo apt-get update && sudo apt-get upgrade` to update all installed packages to their latest versions, including Bash.

#### In Mac

**To update Bash on MacOS:**

Install Homebrew, a package manager for MacOS, by pasting the following command in the Terminal:

```
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

**Post-Installation Configuration for Homebrew on MacOS**

After installing Homebrew, perform the following steps to ensure it's properly set up and integrated into your shell environment:

Determine the installation path of Homebrew by running:

```
HOMEBREW_PREFIX=$(brew --prefix)
```

Configure your shell environment:

For Zsh users (default on macOS Catalina and later):

```
echo "eval \"\$($HOMEBREW_PREFIX/bin/brew shellenv)\"" >> ~/.zprofile
eval "$($HOMEBREW_PREFIX/bin/brew shellenv)"
```

For Bash users (default on macOS Mojave and earlier):

```
echo "eval \"\$($HOMEBREW_PREFIX/bin/brew shellenv)\"" >> ~/.bash_profile
eval "$($HOMEBREW_PREFIX/bin/brew shellenv)"
```

Verify that Homebrew is set up correctly:

```
brew doctor
```

The brew doctor command will check your Homebrew setup and confirm that everything is configured properly.

**Notes:**

This method dynamically finds the Homebrew installation path, ensuring compatibility with both Intel and Apple Silicon Macs.

Close and reopen your terminal after running these commands to apply the changes to new terminal sessions.

You can confirm which shell you're using by executing echo $SHELL in the terminal.

**Once Homebrew is installed, update Bash by running:**

```
brew install bash
```

Add the new Bash to your list of shells:

```
sudo bash -c 'echo /usr/local/bin/bash >> /etc/shells'
```

Change your default shell to the new Bash:

```
chsh -s /usr/local/bin/bash
```

#### In Ubuntu (and other Linux Distributions)

Update your package lists:

```
sudo apt-get update
```

Upgrade Bash to the latest version available in your repositories:

```
sudo apt-get install --only-upgrade bash
```

Install missing commands as needed using apt-get, for example:

```
sudo apt-get install jq lz4 curl
```

### Note on Package Managers

Windows: WSL users can utilize Linux package managers like apt for Ubuntu. Git Bash users might rely on installing Unix command utilities through Git for Windows.

Mac: Homebrew (brew) is recommended for installing Unix tools and updating Bash.

Ubuntu/Linux: The native package manager (apt for Ubuntu, dnf for Fedora, pacman for Arch) is used for installations and updates.

### Running Bash Scripts

#### On Windows

To run bash scripts on Windows, you will need to use a Unix-like environment. The most common approach is to install Git Bash or enable the Windows Subsystem for Linux (WSL).

**Git Bash:**

- Download and install Git for Windows from https://git-scm.com/download/win.
- During installation, ensure you select the option to use Git and optional Unix tools from the Windows Command Prompt.
- After installation, you can right-click in any folder in Windows Explorer and select "Git Bash Here" to open a Bash terminal in that directory.

**Windows Subsystem for Linux (WSL):**

- Open PowerShell as Administrator and run: `wsl --install`.
- Follow the instructions to complete the installation of your preferred Linux distribution from the Microsoft Store.
- Once installed, you can access Linux terminals directly from Windows.

For detailed instructions on setting up WSL, visit the Microsoft documentation: https://learn.microsoft.com/en-us/windows/wsl/install.

#### On Mac

MacOS comes with a built-in Terminal application that supports running bash scripts out-of-the-box.

- Open the Terminal app from your Applications folder or by using Spotlight search (`Cmd + Space`, then type "Terminal").
- Navigate to the directory containing your bash script using the `cd` command.
- To run your script, type `bash script_name.sh`, replacing "script_name.sh" with the name of your script.

Ensure you have the necessary permissions to execute the script. If not, run `chmod +x script_name.sh` before executing it.