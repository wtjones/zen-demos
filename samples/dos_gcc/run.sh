#!/bin/bash

set -euo pipefail

main() {

    source $DJGPP_PREFIX/setenv

    gcc main.c -o build/a.exe

    if [ $? -ne 0 ]; then
        echo "run.sh - build failed"
        exit 1
    fi

    echo "run.sh - build succeeded"
    dosbox -conf dosbox.conf
}

main
