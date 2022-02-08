#!/bin/bash

set -eo pipefail

d=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

make build-builder

# Generate swagger.json
docker run --rm -it \
  --mount type=bind,source="${d}/api",target="/home/haskeller/api" \
  visortelle/haskell-playground-sandbox-builder:main bash -c "set -e; cd /home/haskeller/api && cabal update && cabal run gen-swagger-json"

# Build UI
cd "${d}/../sandbox-ui" && make build
cp -r ${d}/../sandbox-ui/dist/* "${d}/ui/"

# Build Docker image
docker build -t visortelle/haskell-playground-sandbox:main "${d}"
