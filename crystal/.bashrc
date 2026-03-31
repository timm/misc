
export EDITOR=nvim
export HISTSIZE=10000
set -o vi                        # vi mode in shell

# ---- prompt --------------------------------------------------------------
PS1='$(basename $(dirname $PWD))/$(basename $PWD) $ '

# ---- aliases -------------------------------------------------------------
alias vi='nvim  -u ./.nvimrc.lua -p '
alias ls='ls -G'                 # colour on mac, use --color on linux
alias ll='ls -lh'
alias ..='cd ..'
alias ...='cd ../..'

PS1='💎 $(basename $(dirname $PWD))/$(basename $PWD) $ '

