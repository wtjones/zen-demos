#!/bin/bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
rm -rf ${SCRIPT_DIR}/build
cmake -S . -DRAS_PLATFORM=ras_sdl -B build
cmake --build build
./build/bin/demo
