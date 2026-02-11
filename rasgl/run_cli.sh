#!/bin/bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

cmake -S . -B build \
  -DRAS_PLATFORM_TYPE=hosted
cmake --build build --target rascli

if [ $? -ne 0 ]; then
    echo "Build failed"
    exit 1
fi

./build/bin/rascli "$@"
