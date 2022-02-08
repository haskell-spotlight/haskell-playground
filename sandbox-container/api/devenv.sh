# source me

d=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

export HSPG_PORT=8080
export HSPG_ORIGIN=localhost:8090
export HSPG_PUBLIC_URL="http://localhost:8090"
export HSPG_SANDBOX_ROOT="${d}/../sandbox-example"
export HSPG_UI_DIST="${d}/../ui"
export HSPG_NGINX_CONFIG_PATH="/tmp/haskell-playground-nginx.conf"
export HSPG_NGINX_PORT=8090
