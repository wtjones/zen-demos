#!/bin/bash

podman build -t mycl .
podman run --rm -it -v .:/shared mycl

