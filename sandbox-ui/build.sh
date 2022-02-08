#!/bin/bash

set -e

this_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

cp $this_dir/../sandbox-container/api/swagger.json ./
set +e; mkdir $this_dir/src/api; set -e;
$this_dir/swagger-to-ts.sh "${this_dir}/swagger.json" "${this_dir}/src/api"

docker build -t haskell-playground-ui-builder -f $this_dir/Dockerfile.builder $this_dir
docker run -it --mount type=bind,source="$(pwd)",target="/src" haskell-playground-ui-builder
