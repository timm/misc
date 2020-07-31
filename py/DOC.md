# Documentation

usage="sh DOC.md"   

Ell=$(cd $( dirname "${BASH_SOURCE[0]}" ) && pwd )  

d=$Ell/docs

mkdir -p $d
  
pdoc3 -o $d --template-dir $d --force --html $Ell/duo.py    
pydoc3 duo | bat -plman 
