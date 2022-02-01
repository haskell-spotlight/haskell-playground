#!/bin/bash
set -e
cd "${HASKELL_SANDBOX_ROOT}"

cabal repl
