#!/bin/bash

set -euo pipefail

DEMO=${1:-world}
DEBUG=${2:-0}
DEMO_PARAM_01=${3:-}

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

${SCRIPT_DIR}/build_deps_dos.sh
${SCRIPT_DIR}/tools/gen_dosbox.sh $DEMO_PARAM_01

main() {

    source $DJGPP_PREFIX/setenv
    SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

    cmake -S . \
        -DCMAKE_TOOLCHAIN_FILE=tools/djgpp.cmake \
        -DRAS_PLATFORM=ras_dos \
        -DRAS_DEMO=$DEMO  \
        -DDEBUG=$DEBUG \
        -B bld_dos
    cmake --build bld_dos -t demo
    if [ $? -ne 0 ]; then
        echo "build failed"
        exit 1
    fi
    echo "build succeeded"

    $DOSBOX_BIN -conf tools/dosbox.conf
}

main
