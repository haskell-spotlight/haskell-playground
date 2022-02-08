#!/bin/bash

docker run -it \
  --mount type=bind,source=$1,target="/local/swagger.json" \
  --mount type=bind,source=$2,target="/local/out" \
  openapitools/openapi-generator-cli:v5.4.0 generate --generator-name typescript-axios --input-spec /local/swagger.json --output /local/out
