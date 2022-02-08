#!/bin/bash

set -eo pipefail

this_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

make build-builder

# Generate swagger.json
docker run -it \
  --mount type=bind,source="${this_dir}/api",target="/home/haskeller/api" \
  visortelle/haskell-playground-sandbox-builder:main bash -c "set -e; cd /home/haskeller/api && cabal update && cabal run gen-swagger-json"

# Build UI
cd "${this_dir}/../sandbox-ui" && make build
cp -r ${this_dir}/../sandbox-ui/dist/* "${this_dir}/ui/"

# Build Docker image
docker build -t visortelle/haskell-playground-sandbox:main "${this_dir}"
