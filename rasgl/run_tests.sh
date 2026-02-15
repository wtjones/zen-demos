#!/bin/bash

DEBUG=${1:-0}

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

cmake -S . -DDEBUG=$DEBUG -DENABLE_ASAN=ON -B build
cmake --build build
./build/bin/tests
