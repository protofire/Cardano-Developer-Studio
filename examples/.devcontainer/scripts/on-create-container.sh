#!/bin/bash

chmod +x /home/plutus/workspace/.devcontainer/scripts/on-start-container.sh

echo "Preparing Container. Executing Cabal Update..."

cabal update

echo "Container Created!"
