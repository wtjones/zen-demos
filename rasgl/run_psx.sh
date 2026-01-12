#!/bin/bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

# Set in shell profile:
# $DUCKSTATION=/home/<user>/Applications/DuckStation-x64_69d63fca1439f8efe7495c105306c59e.AppImage

if [ -z "${DUCKSTATION:-}" ]; then
    echo "DUCKSTATION is not set. Please set the path to DuckStation emulator in the DUCKSTATION environment variable."
    exit 1
fi

$DUCKSTATION $SCRIPT_DIR/bld_psx/ras_psx.psexe
