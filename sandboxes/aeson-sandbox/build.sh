#!/bin/bash

set -eo pipefail

cabal update
cabal build
