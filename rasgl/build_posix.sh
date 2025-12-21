#!/bin/bash

DEMO=${1:-poly}
DEBUG=${2:-0}

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cmake -S . \
  -DRAS_PLATFORM=ras_sdl \
  -DRAS_DEMO=$DEMO \
  -DDEBUG=$DEBUG \
  -DENABLE_ASAN=ON \
  -B build
cmake --build build
