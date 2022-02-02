this_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

export HSPG_PORT=8090
export HSPG_ORIGIN=localhost:4242
export HSPG_PUBLIC_URL=localhost:8090
export HSPG_SANDBOX_ROOT="${this_dir}/../../sandboxes/lesson-task-example-sandbox"
