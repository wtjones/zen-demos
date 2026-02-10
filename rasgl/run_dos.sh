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

    if [ "$DEBUG" -eq 1 ]; then
        BUILD_TYPE="Debug"
    else
        BUILD_TYPE="Release"
    fi

    cmake -S . \
        -DCMAKE_TOOLCHAIN_FILE=tools/djgpp.cmake \
        -DCMAKE_BUILD_TYPE=$BUILD_TYPE \
        -DRAS_PLATFORM=ras_dos \
        -DRAS_DEMO=$DEMO  \
        -DDEBUG=$DEBUG \
        -DRAS_LOG_BUFFER_MODE=${RAS_LOG_BUFFER_MODE:-0} \
        -B bld_dos
    cmake --build bld_dos -t demo
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
