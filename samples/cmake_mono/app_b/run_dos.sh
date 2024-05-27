#!/bin/bash

set -euo pipefail


SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

main() {

    source $DJGPP_PREFIX/setenv
    SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

    LIB_A_DIR=${SCRIPT_DIR}/../lib_a

    rm -rf ${LIB_A_DIR}/build
    pushd ${LIB_A_DIR}
    cmake -S . -DCMAKE_TOOLCHAIN_FILE=${SCRIPT_DIR}/tools/djgpp.cmake -B build
    cmake --build build
    popd

    rm -rf ${SCRIPT_DIR}/build
    cmake -S . -DCMAKE_TOOLCHAIN_FILE=tools/djgpp.cmake -B build
    cmake --build build
    if [ $? -ne 0 ]; then
        echo "build failed"
        exit 1
    fi
    echo "build succeeded"

    $DOSBOX_BIN -conf tools/dosbox.conf
}

main
