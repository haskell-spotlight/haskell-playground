#!/bin/bash
set -e
cd "${HSPG_SANDBOX_ROOT}"

cabal update
cabal build
