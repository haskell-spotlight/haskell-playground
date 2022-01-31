#!/bin/bash

set -eo pipefail

envsubst '${HSPG_PUBLIC_URL} ${HSPG_ORIGIN}' </etc/nginx/template.nginx.conf >/etc/nginx/nginx.conf

nginx &

# Expose shell terminal
gotty \
  --port 9000 \
  --permit-write \
  --ws-origin "${HSPG_ORIGIN}" \
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
  --ws-origin "${HSPG_ORIGIN}" \
  --width 140 \
  --height 80 \
  --term xterm \
  --close-signal 9 \
  ./repl.sh
