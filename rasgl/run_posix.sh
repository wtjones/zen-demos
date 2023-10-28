#!/bin/bash

DEMO=${1:-world}
DEBUG=${2:-0}

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
rm -rf ${SCRIPT_DIR}/build
cmake -S . -DRAS_PLATFORM=ras_sdl -DRAS_DEMO=$DEMO -DDEBUG=$DEBUG -B build
cmake --build build
./build/bin/demo
