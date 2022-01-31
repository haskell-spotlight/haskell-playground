#!/bin/bash

set -eo pipefail

export EDITOR=nano
export TERM_WIDTH=140
export TERM_HEIGHT=80

envsubst '${HSPG_PUBLIC_URL} ${HSPG_ORIGIN}' </etc/nginx/template.nginx.conf >/etc/nginx/nginx.conf

nginx &

# Expose editor
gotty \
  --width $TERM_WIDTH \
  --height $TERM_HEIGHT \
  --reconnect \
  --reconnect-time 600 \
  --port 8999 \
  --permit-write \
  --ws-origin "${HSPG_ORIGIN}" \
  --close-signal 9 \
  $EDITOR ./app/Main.hs \
  &

# Expose shell terminal
gotty \
  --width $TERM_WIDTH \
  --height $TERM_HEIGHT \
  --reconnect \
  --reconnect-time 600 \
  --port 9000 \
  --permit-write \
  --ws-origin "${HSPG_ORIGIN}" \
  --close-signal 9 \
  zsh \
  &

# Expose ghci terminal
gotty \
  --width $TERM_WIDTH \
  --height $TERM_HEIGHT \
  --reconnect \
  --reconnect-time 600 \
  --port 9001 \
  --permit-write \
  --ws-origin "${HSPG_ORIGIN}" \
  --close-signal 9 \
  ./repl.sh
