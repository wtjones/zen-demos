#!/bin/bash

set -euo pipefail

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

main() {

    CMAKE_CMD="cmake \
        -DCMAKE_TOOLCHAIN_FILE=tools/djgpp.cmake \
        -B bld_dos"

    docker run -i --rm \
        --user $(id -u):$(id -g) \
        -v "$SCRIPT_DIR":/work/r \
        -w /work/r \
        --name larse-dos-app larse-dos \
        /bin/bash -c "${CMAKE_CMD} && cmake --build bld_dos"

    if [ $? -ne 0 ]; then
        echo "build failed"
        exit 1
    fi
    echo "build succeeded"
}

main
