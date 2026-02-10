#!/bin/bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

if [ -n "${RAS_MAX_FRAMES:-}" ]; then
	RAS_ARG="-DRAS_MAX_FRAMES=${RAS_MAX_FRAMES}"
else
	RAS_ARG=""
fi

if [ -n "${RAS_LOG_BUFFER_MODE:-}" ]; then
	RAS_ARG="${RAS_ARG} -DRAS_LOG_BUFFER_MODE=${RAS_LOG_BUFFER_MODE}"
else
	RAS_ARG="${RAS_ARG} -DRAS_LOG_BUFFER_MODE=0"
fi

CMAKE_CMD="cmake \
	-DCMAKE_TOOLCHAIN_FILE=/work/r/cmake-psx/toolchain.cmake \
	-DVENV_PATH=/work/env \
	-DCMAKE_BUILD_TYPE=Debug \
	-DRAS_PLATFORM=ras_psx \
	-DRAS_DEMO=mini \
	${RAS_ARG} \
	-GNinja \
	-B bld_psx"

docker run -it --rm \
	--user $(id -u):$(id -g) \
	-v "$SCRIPT_DIR":/work/r \
	-w /work/r \
	--name psx-app ras-psx \
	/bin/bash -c "${CMAKE_CMD} && cmake --build bld_psx"
