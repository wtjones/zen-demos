#!/bin/bash

set -euo pipefail

main() {

    source $DJGPP_PREFIX/setenv    
    SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

    echo "building larse"
    
    pushd ${SCRIPT_DIR}/../larse
    cmake -S . -DCMAKE_TOOLCHAIN_FILE=tools/djgpp.cmake -B build
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
