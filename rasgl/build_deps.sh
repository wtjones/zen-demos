#!/bin/bash

set -euo pipefail

main() {

    SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

    echo "building larse"
    BUILD_DIR="${SCRIPT_DIR}/../larse/build"

    if [ -d "$BUILD_DIR" ]; then
        echo "Deleting existing build directory: $BUILD_DIR"
        rm -rf -- "$BUILD_DIR"
    fi

    pushd ${SCRIPT_DIR}/../larse
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
