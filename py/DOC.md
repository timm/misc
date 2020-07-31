# Documentation

usage="sh DOC.md"   

Ell=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)  

d=$Ell/docs   

mkdir -p $d  
  
# cat $Ell/duo.py |
# awk '/^"""/ {i++}     
#             {s=""  
#              if(i==1)
#                if($0!~/  $/) 
#                  if ($0 ~ /:$/)
#                     s="  ";
#              print $0 s}   
# ' > $$
# mv $$ $Ell/duo.py

pdoc3 -o $d --template-dir $d --force --html $Ell/duo.py    

#pydoc3 duo | bat -plman 
