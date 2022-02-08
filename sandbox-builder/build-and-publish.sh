#!/bin/bash

set -eo pipefail

this_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

sandbox_name="${1}"

"${this_dir}/build.sh" "${sandbox_name}"

docker push "visortelle/haskell-playground-sandbox:${sandbox_name}"
