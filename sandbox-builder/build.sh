#!/bin/bash

set -eo pipefail

repo_root_dir=$(git rev-parse --show-toplevel)
d=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

sandboxes_dir="${d}/../sandboxes"
sandbox_name="${1}"
sandbox_dir="${sandboxes_dir}/${sandbox_name}"

docker build -f "${d}/Dockerfile" -t "visortelle/haskell-playground-sandbox:${sandbox_name}" "$sandbox_dir"
