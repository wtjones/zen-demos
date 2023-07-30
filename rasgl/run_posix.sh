#!/bin/bash

DEBUG=${1:-0}

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
rm -rf ${SCRIPT_DIR}/build
cmake -S . -DRAS_PLATFORM=ras_sdl -DDEBUG=$DEBUG -B build
cmake --build build
./build/bin/demo
