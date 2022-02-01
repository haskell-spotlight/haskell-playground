#!/bin/bash

set -e
this_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

"${this_dir}/../tasks/Run tests.sh"
