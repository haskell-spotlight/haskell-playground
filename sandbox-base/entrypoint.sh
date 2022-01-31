#!/bin/bash

set -eo pipefail

nginx &

# Expose shell terminal
gotty \
  --port 9000 \
  --permit-write \
  --ws-origin "localhost:8080" \
  --width 140 \
  --height 80 \
  --term xterm \
  --close-signal 9 \
  zsh \
  &

# Expose ghci terminal
gotty \
  --port 9001 \
  --permit-write \
  --ws-origin "localhost:8080" \
  --width 140 \
  --height 80 \
  --term xterm \
  --close-signal 9 \
  ./repl.sh
