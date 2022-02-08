#!/bin/bash

set -eo pipefail

repo_root_dir=$(git rev-parse --show-toplevel)
this_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

sandboxes_dir="${this_dir}/../sandboxes"
sandbox_name="${1}"
sandbox_dir="${sandboxes_dir}/${sandbox_name}"

docker build -f "${this_dir}/Dockerfile" -t "visortelle/haskell-playground-sandbox:${sandbox_name}" "$sandbox_dir"
