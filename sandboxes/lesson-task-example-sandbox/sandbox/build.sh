#!/bin/bash
set -e
cd "${HASKELL_SANDBOX_ROOT}"

cabal update
cabal build
