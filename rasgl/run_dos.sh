#!/bin/bash

set -euo pipefail

main() {

    source $DJGPP_PREFIX/setenv
    cmake -S . -DCMAKE_TOOLCHAIN_FILE=tools/djgpp.cmake -B build
    cmake --build build -t ras_dos
    if [ $? -ne 0 ]; then
        echo "build failed"
        exit 1
    fi
    echo "build succeeded"

    $DOSBOX_BIN -conf tools/dosbox.conf
}

main
