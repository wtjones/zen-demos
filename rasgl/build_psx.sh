#!/bin/bash

set -euo pipefail

DEMO=${1:-poly}
DEBUG=${2:-0}

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
BUILD_DIR="${SCRIPT_DIR}/bld_psx"
ELF_PATH="${BUILD_DIR}/ras_psx.elf"

if [ ! -d "$BUILD_DIR" ]; then
	mkdir -p "$BUILD_DIR"
fi

# Pack the scene data for embedded platforms.
${SCRIPT_DIR}/run_cli.sh -p $3 -o ${BUILD_DIR}/scene.mp

if [ $? -ne 0 ]; then
  echo "run_cli.sh failed" >&2
  exit 1
fi

if [ -n "${RAS_MAX_FRAMES:-}" ]; then
	RAS_ARG="-DRAS_MAX_FRAMES=${RAS_MAX_FRAMES}"
else
	RAS_ARG=""
fi

if [ -n "${RAS_USE_MAIN_MIN:-}" ]; then
	RAS_ARG="${RAS_ARG} -DRAS_USE_MAIN_MIN=${RAS_USE_MAIN_MIN}"
else
	RAS_ARG=""
fi

if [ -n "${RAS_LOG_BUFFER_MODE:-}" ]; then
	RAS_ARG="${RAS_ARG} -DRAS_LOG_BUFFER_MODE=${RAS_LOG_BUFFER_MODE}"
else
	RAS_ARG="${RAS_ARG} -DRAS_LOG_BUFFER_MODE=0"
fi

if [ "$DEBUG" -eq 1 ]; then
	BUILD_TYPE="Debug"
else
	BUILD_TYPE="Release"
fi

CMAKE_CMD="cmake \
	-DCMAKE_TOOLCHAIN_FILE=/work/r/cmake-psx/toolchain.cmake \
	-DVENV_PATH=/work/env \
	-DCMAKE_BUILD_TYPE=${BUILD_TYPE} \
	-DRAS_PLATFORM=ras_psx \
	-DRAS_DEMO=${DEMO} \
	${RAS_ARG} \
	-GNinja \
	-B bld_psx"

docker run -it --rm \
	--user $(id -u):$(id -g) \
	-v "$SCRIPT_DIR":/work/r \
	-w /work/r \
	--name psx-app ras-psx \
	/bin/bash -c "${CMAKE_CMD} && cmake --build bld_psx"

if [ -f "$ELF_PATH" ]; then
  size "$ELF_PATH"
fi
