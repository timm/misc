# SHELL Tricks

usage=". SHELL.md" 

Ell=$(cd $( dirname "${BASH_SOURCE[0]}" ) && pwd )  

here()  { cd $1; basename `pwd`; }    

alias gg="git pull"   
alias gs="git status"   
alias gp="git commit -am 'saving'; git push; git status"    
alias ok="pytest.py  $Ell/duo.py"     
alias spy="rerun 'pytest duo.py'"    
alias doc="sh $Ell/DOC.md"  

ok1() { pytest.py -s -k $1 $Ell/duo.py; }  


PROMPT_COMMAND='echo -ne "🔆 $(git branch 2>/dev/null | grep '^*' | colrm 1 2):";PS1="$(here ..)/$(here .):\!\e[m ▶ "'   


