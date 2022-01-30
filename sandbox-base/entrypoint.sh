#!/bin/bash

set -eo pipefail

# Expose shell terminal
gotty --port 8000 --permit-write --width 140 --height 80 --term xterm --close-signal 9 zsh &

# Expose ghci terminal
gotty --port 8001 --permit-write --width 140 --height 80 --term xterm --close-signal 9 repl.sh
