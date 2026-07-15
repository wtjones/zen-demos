#!/bin/bash

DEMO=${1:-poly}
DEBUG=${2:-0}
SCENE=${3:-assets/scenes/multi01.lsp}

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

${SCRIPT_DIR}/build_posix.sh $DEMO $DEBUG

if [ $? -ne 0 ]; then
    echo "Build failed"
    exit 1
fi

ASAN_OPTIONS=detect_leaks=0 ./build/bin/demo $SCENE
