#!/bin/bash

set -euo pipefail

DEBUG=${1:-0}

main() {

    source $DJGPP_PREFIX/setenv
    SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
    rm -rf ${SCRIPT_DIR}/build
    cmake -S . -DCMAKE_TOOLCHAIN_FILE=tools/djgpp.cmake -DRAS_PLATFORM=ras_dos -DDEBUG=$DEBUG -B build
    cmake --build build -t demo
    if [ $? -ne 0 ]; then
        echo "build failed"
        exit 1
    fi
    echo "build succeeded"

    $DOSBOX_BIN -conf tools/dosbox.conf
}

main
