#!/bin/bash

set -euo pipefail


SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

main() {

    source $DJGPP_PREFIX/setenv
    SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

    cmake -S . \
        -DCMAKE_TOOLCHAIN_FILE=tools/djgpp.cmake \
        -B bld_dos
    cmake --build bld_dos
    if [ $? -ne 0 ]; then
        echo "build failed"
        exit 1
    fi
    echo "build succeeded"

    if [ -z "${DOSBOX_BIN:-}" ]; then
        echo "DOSBOX_BIN is not set, using flatpak fallback."
        flatpak run --filesystem=/tmp --filesystem="$(pwd)" com.dosbox_x.DOSBox-X -conf tools/dosbox.conf
    else
        $DOSBOX_BIN -conf tools/dosbox.conf
    fi
}

main
