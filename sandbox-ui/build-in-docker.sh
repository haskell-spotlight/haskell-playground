#!/bin/bash

set -e

docker build -t haskell-playground-ui-builder -f ./Dockerfile.builder .
docker run -it --mount type=bind,source="$(pwd)",target="/src" haskell-playground-ui-builder
