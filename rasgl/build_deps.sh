#!/bin/bash

set -euo pipefail

main() {

    echo "building larse"
    pushd ../larse
    cmake -S . -B build
    cmake --build build
    if [ $? -ne 0 ]; then
        echo "larse build failed"
        popd
        exit 1
    fi
    popd
    echo "larse build succeeded"
}

main
