#!/bin/bash

set -e

d=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

cp $d/../sandbox-container/api/swagger.json ./

set +e
mkdir $d/src/api
set -e

$d/swagger-to-ts.sh "${d}/swagger.json" "${d}/src/api"

docker build -t haskell-playground-ui-builder -f $d/Dockerfile.builder $d
docker run --rm -it --mount type=bind,source="$(pwd)",target="/src" haskell-playground-ui-builder
