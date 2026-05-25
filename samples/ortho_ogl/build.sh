#!/usr/bin/env bash
set -euo pipefail

# Usage: ./build.sh [debug]
# Example: ./build.sh 0    # Release (default)
# Example: ./build.sh 1    # Debug

BIN_NAME=ortho_ogl
DEBUG=${1:-0}

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
BUILD_DIR="$SCRIPT_DIR/build"

if [ "$DEBUG" -ne 0 ]; then
  CMAKE_BUILD_TYPE=Debug
else
  CMAKE_BUILD_TYPE=Release
fi

echo "Configuring ${BIN_NAME} (build type: ${CMAKE_BUILD_TYPE}) in ${BUILD_DIR}"

cmake -S "$SCRIPT_DIR" -B "$BUILD_DIR" \
  -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE} \
  ${CMAKE_ARGS:-}

cmake --build "$BUILD_DIR" --config ${CMAKE_BUILD_TYPE} -- -j$(nproc)

echo "Build finished. Binary is in ${BUILD_DIR}/"
