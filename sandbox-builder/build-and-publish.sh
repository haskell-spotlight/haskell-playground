#!/bin/bash

set -eo pipefail

d=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

sandbox_name="${1}"

"${d}/build.sh" "${sandbox_name}"

docker push "visortelle/haskell-playground-sandbox:${sandbox_name}"
