#!/bin/bash

SESSION="ras_session"

if ! tmux has-session -t "$SESSION" 2>/dev/null; then
    tmux new-session -d -s "$SESSION"
fi

tmux split-window -v -t "$SESSION"
tmux send-keys -t "$SESSION":0.1 'touch /tmp/rasgl.log && tail -f /tmp/rasgl.log' C-m
tmux select-pane -t "$SESSION":0.0

if [ -n "$TMUX" ]; then
    exec tmux switch-client -t "$SESSION"
else
    exec tmux attach-session -t "$SESSION"
fi
