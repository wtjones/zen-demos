#!/bin/bash

DEMO=${1:-world}
DEBUG=${2:-0}

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

${SCRIPT_DIR}/build_posix.sh $DEMO $DEBUG

if [ $? -ne 0 ]; then
    echo "Build failed"
    exit 1
fi

ASAN_OPTIONS=detect_leaks=0 ./build/bin/demo $3
