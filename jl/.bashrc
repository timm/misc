root=$(git rev-parse --show-toplevel)
ishs=$HOME/tmp/ish
mkdir -p $ishs
 
alias vi="vim -u $ishs/.vimrc "
alias ..='cd ..'
alias tmux="$(which tmux) -f $ishs/.tmuxrc "
alias python="$(which python3) -B "
alias matrix="$(which cmatrix) -bs  -C blue"
alias rain="$(which rain) -d 60 "
alias tree="$(which tree) -C "
alias mc="$(which mc) -x "
alias zap='rm -rf $root/raised/\.*.swp $root/raised/*.pyc $root/raised/__pycache__'
alias add='zap; git add *'
alias put='gfig; zap; git commit -am saving; git push; git status'
alias get='gfig; git pull'
vim -u $ishs/.vimrc +PlugInstall  +qall
uname="$(uname -s)"
case "${uname}" in
     Linux*)
       alias ls='ls --color=auto';;
     Darwin*)
       alias ls='ls -G' ;;
     *) ;;
esac
_c0="\033[00m"     # white
_c1="\033[01;32m"  # green
 	_c2="\033[01;34m"  # blue
 	_c3="\033[31m"     # red
 	_c5="\033[35m"     # purple
 	_c6="\033[33m"     # yellow
 	_c7="\033[36m"     # turquoise _c8="\033[96m"     # magenta
 
ok()  { (cd $root/test; python ${1}.py;) }
py()  { (cd $root/test; python ${1}.py;) }
 
mds() {
   (
   cd $root/raised
           git rm -qrf ../docs/* 
           pdoc --force --template-dir ../etc/templates --html --output-dir  ../docs  ./ 
           cd ../docs
           mv raised/* .
           cp ../etc/img/r.ico .
 	   echo .nojekyll > .nojekyll
           git add .nojekyll 
           git add r.ico
           git add * 
           git add */*
   #pdoc --template-dir $root/etc/templates --html --output-dir  $root/docs --force  *
   #pdoc --html --output-dir  $root/docs --force  ./
   )
}
 
gfig() {
   git config --global credential.helper cache
   git config credential.helper 'cache --timeout=3600'
}
 
reload() { . $ishs/.bashrc ; }
 
  here() { cd $1; basename "$PWD"; }
  PROMPT_COMMAND='echo -ne "${_c2}iSH:${_c6}$(git branch 2>/dev/null | grep '^*' | colrm 1 2) ";PS1="${_c1}$_c0$(here ../..)/$(here ..)/$(here .) ${_c3}\!>${_c0}\e[m "'
