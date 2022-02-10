#!/bin/bash

set -eo pipefail

docker run \
  --rm \
  -it \
  -p 8090:8090 \
  -e HSPG_ORIGIN="localhost:8090" \
  -e HSPG_PUBLIC_URL="http://localhost:8090" \
  -e HSPG_SANDBOX_ROOT="/home/haskeller/Playground" \
  --mount type=bind,source="$(pwd)/Playground",target="/home/haskeller/Playground" \
  --mount type=bind,source="$(pwd)/../sandbox-ui/dist",target="/home/haskeller/ui" \
  visortelle/haskell-playground-sandbox:main
