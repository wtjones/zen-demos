#!/bin/bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

if [ -n "${RAS_MAX_FRAMES}" ]; then
	RAS_ARG="-DRAS_MAX_FRAMES=${RAS_MAX_FRAMES}"
else
	RAS_ARG=""
fi

docker run -it --rm \
 	--user $(id -u):$(id -g) \
	-v "$SCRIPT_DIR":/work/r \
	-w /work/r \
	--name psx-app ras-psx \
	/bin/bash -c "cmake -DCMAKE_TOOLCHAIN_FILE=/work/r/cmake-psx/toolchain.cmake -DVENV_PATH=/work/env -DCMAKE_BUILD_TYPE=Debug -DRAS_PLATFORM=ras_psx -DRAS_DEMO=mini ${RAS_ARG} -GNinja -B bld_psx && cmake --build bld_psx"
