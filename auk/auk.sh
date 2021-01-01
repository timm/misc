#!/usr/bin/env bash
### configuration
##  where to write the generated code
Lib=$HOME/opt/share/awk
### end configuration

usage() {  tput bold; tput setaf 6; cat <<'EOF'
AUK: a preprocessor for AWK code
(c) 2020 MIT License, Tim Menzies timm@ieee.org

         .-"-.
       /  ,~a\_
       \  \__))>  a little auk (awk) goes a long way
       ,) ." \    
      /  (    \
     /   )    ;
    /   /     /
  ,/_."`  _.-`
   /_/`"\\___
        `~~~`

EOF
  tput sgr0; tput setaf 7; cat<<'EOF'
Augments standard gawk with polymorphism, encapsulation, 
objects, attributes, methods, iterators, multi-line comments.

USAGE:

  ./auk.sh                converts all .auk files to .awk 
  ./auk.sh -h             as above, also prints this help text
  ./auk.sh xx.auk         as above, then runs xx.awk
  ./auk.sh xx             ditto
  Com | ./auk.sh xx.auk   as above, taking input from Com
  Com | ./auk.sh xx       ditto
  . auk.sh                adds some bash tools to local enviroment

Alternatively, to execute your source file directly using ./xx.auk,
chmod +x xx.auk and add the top line:

  #!/usr/bin/env path2auk.sh

If called via ". auk.sh" then the following alias are defined:
EOF
  tput setaf 9; echo ""
  gawk 'sub(/^[ \t]*alias/,"alias") {print $0}' $Auk/auk.sh
  tput sgr0
}

mkdir -p $Lib
Auk=$(cd $( dirname "${BASH_SOURCE[0]}" ) && pwd )

cp $Auk/auk.awk $Lib

for i in *.auk; do
  f=$i
  g=$(basename $f)
  g=$Lib/${g%.*}.awk
  if [ "$f" -nt "$g" ]; then 
     gawk -f $Lib/auk.awk --source 'BEGIN { auk2awk("'$f'")}' > $g 
  fi
done

if [ "$1" == "-h" ]; then
  usage
elif [ -n "$1" ]; then
  f=$1
  g=$(basename $f)
  g=$Lib/${g%.*}.awk
  shift
  AWKPATH="$Lib:./:$AWKPATH"
  COM="gawk -f $Lib/auk.awk -f $g $*"
  if [ -t 0 ]
    then         AWKPATH="$AWKPATH" $COM
    else cat - | AWKPATH="$AWKPATH" $COM
  fi
else
  alias auk='bash $Auk/auk.sh '                 # short cut to this code
  alias gp='ga;git commit -am save;git push;gs' # end-of-day actions
  alias gs='git status'                         # status 
  alias ls='ls -G'                              # ls
  alias reload='. $Auk/auk'                     # reload these tools
fi
