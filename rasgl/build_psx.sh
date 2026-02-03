#!/bin/bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

docker run -it --rm \
 	--user $(id -u):$(id -g) \
	-v "$SCRIPT_DIR":/work/r \
	-w /work/r \
	--name psx-app ras-psx \
	/bin/bash -c "
	  cmake -DCMAKE_TOOLCHAIN_FILE=/work/r/cmake-psx/toolchain.cmake -DVENV_PATH=/work/env -DCMAKE_BUILD_TYPE=Debug -DRAS_PLATFORM=ras_psx -GNinja -B bld_psx &&
	  cmake --build bld_psx
	"
