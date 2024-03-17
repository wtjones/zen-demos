#!/bin/bash

# Generate the dosbox.conf using a heredoc template
# https://stackoverflow.com/a/12006837/107161

DEMO_PARAM_01=${1:-}
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

cat_template() {
    echo "cat << EOT"
    cat ${SCRIPT_DIR}/dosbox-template.conf
    echo EOT
 }

cat_template | DEMO_PARAM_01=$DEMO_PARAM_01 bash > ${SCRIPT_DIR}/dosbox.conf
