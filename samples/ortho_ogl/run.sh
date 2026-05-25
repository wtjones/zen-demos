#!/usr/bin/env bash
set -euo pipefail

# Usage: ./run.sh [debug] [args...]
# Example: ./run.sh            # rebuilds (Release) and runs ortho_ogl
# Example: ./run.sh 1          # rebuilds (Debug) and runs ortho_ogl

# This script always builds and runs the project's demo binary `ortho_ogl`.
BIN_NAME=ortho_ogl

# Parse optional numeric debug flag as first positional argument
DEBUG=0
if [ "${1-}" ]; then
  case "$1" in
    ''|*[!0-9]* ) ;;
    * ) DEBUG=$1; shift || true ;;
  esac
fi

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
BUILD_DIR="$SCRIPT_DIR/build"
BIN_PATH="$BUILD_DIR/$BIN_NAME"

echo "Rebuilding ${BIN_NAME} (debug=${DEBUG})..."
"$SCRIPT_DIR/build.sh" "$BIN_NAME" "$DEBUG"

if [ ! -x "$BIN_PATH" ]; then
  echo "Build finished but binary $BIN_PATH not found or not executable" >&2
  exit 1
fi

echo "Running $BIN_PATH $*"
exec "$BIN_PATH" "$@"
