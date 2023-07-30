#!/bin/bash

DEBUG=${1:-0}

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
rm -rf ${SCRIPT_DIR}/build
cmake -S . -DDEBUG=$DEBUG -B build
cmake --build build
./build/bin/tests
