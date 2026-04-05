PS1='👱 $(basename $(dirname $PWD))/$(basename $PWD) $ '
alias julia='env JULIA_STACKTRACE_MINIMAL=true julia --compile=min --startup-file=no -O0'
