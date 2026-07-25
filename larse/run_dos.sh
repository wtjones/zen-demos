#!/bin/bash

set -euo pipefail

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

main() {

    ${SCRIPT_DIR}/build_dos.sh

    if [ $? -ne 0 ]; then
        echo "build failed"
        exit 1
    fi
    echo "build succeeded"

    if [ -z "${DOSBOX_BIN:-}" ]; then
        echo "DOSBOX_BIN is not set, using flatpak fallback."
        flatpak run --filesystem=/tmp --filesystem="$(pwd)" com.dosbox_x.DOSBox-X -conf tools/dosbox.conf
    else
        $DOSBOX_BIN -conf tools/dosbox.conf
    fi
}

main
