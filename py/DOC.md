# Documentation

usage="sh DOC.md"   

Ell=$(cd $( dirname "${BASH_SOURCE[0]}" ) && pwd )  
  
pdoc3 --template-dir $Ell/html/ --force --html $Ell/duo.py   
pydoc3 duo | bat -plman  
