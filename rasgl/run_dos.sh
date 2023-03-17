#!/bin/bash

set -euo pipefail

main() {

    source $DJGPP_PREFIX/setenv
    rm -rf build
    cmake -S . -DCMAKE_TOOLCHAIN_FILE=tools/djgpp.cmake -DRAS_PLATFORM=ras_dos -B build
    cmake --build build -t demo
    if [ $? -ne 0 ]; then
        echo "build failed"
        exit 1
    fi
    echo "build succeeded"

    $DOSBOX_BIN -conf tools/dosbox.conf
}

main
