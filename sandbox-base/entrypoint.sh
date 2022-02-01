#!/bin/bash

set -eo pipefail

export EDITOR=micro
export HASKELL_SANDBOX_ROOT=$(pwd)

envsubst '${HSPG_PUBLIC_URL} ${HSPG_ORIGIN}' </etc/nginx/template.nginx.conf >/etc/nginx/nginx.conf

nginx &

# Expose editor
gotty \
  --reconnect \
  --reconnect-time 600 \
  --port 8999 \
  --permit-write \
  --ws-origin "${HSPG_ORIGIN}" \
  --close-signal 9 \
  --term xterm \
  $EDITOR ./app/Main.hs \
  &

# Expose shell terminal
gotty \
  --reconnect \
  --reconnect-time 600 \
  --port 9000 \
  --permit-write \
  --ws-origin "${HSPG_ORIGIN}" \
  --close-signal 9 \
  --term xterm \
  zsh \
  &

# Expose ghci terminal
gotty \
  --reconnect \
  --reconnect-time 600 \
  --port 9001 \
  --permit-write \
  --ws-origin "${HSPG_ORIGIN}" \
  --close-signal 9 \
  --term xterm \
  ./repl.sh
