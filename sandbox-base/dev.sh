#!/bin/bash

set -eo pipefail

docker run \
  -it \
  -p 8080:8080 \
  -p 8090:8090 \
  -e HSPG_ORIGIN="localhost:8090" \
  -e HSPG_PUBLIC_URL="http://localhost:8090" \
  --mount type=bind,source="$(pwd)/test-sandbox",target="/home/haskeller/sandbox" \
  visortelle/haskell-playground-sandbox-base:main
