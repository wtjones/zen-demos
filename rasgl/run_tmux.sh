#!/bin/bash

tmux new-session -d -s ras_session
tmux split-window -v
tmux select-pane -t 1
tmux send-keys "touch /tmp/rasgl.log && tail -f /tmp/rasgl.log" C-m
tmux select-pane -t 0
tmux attach-session -t ras_session
