echo 0| sudo tee /proc/sys/kernel/yama/ptrace_scope
./run_build.sh && ./build/bin/rexile
