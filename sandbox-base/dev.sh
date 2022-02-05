#!/bin/bash

set -eo pipefail

docker run -it -p 8080:8080 -p 8090:8090 \
  -e HSPG_ORIGIN="localhost:8080" \
  -e HSPG_PUBLIC_URL="http://localhost:8080" \
  --mount type=bind,source="$(pwd)/sandbox-example",target="/home/haskeller/sandbox" \
  visortelle/haskell-playground-sandbox-base:main
