###               __                 
###              /\ \                
###       __     \ \ \/'\      ___   
###     /'__`\    \ \ , <     / __`\ 
###    /\ \L\.\_   \ \ \\`\  /\ \L\ \
###    \ \__/.\_\   \ \_\ \_\\ \____/
###     \/__/\/_/    \/_/\/_/ \/___/ 

import re,tricks

def good(x, nump): 
  return (type(x)==int or type(x)== float) if nump else (type(x)==str)

def make(x,nump):
  return (tricks.atom(x) if nump else x)

def nump(s):   return re.search(r"^[A-Z]",s) 
def skipp(s):  return re.search(r":$",s)
def lessp(s):  return re.search(r"^-$]",s)
def goalp(s):  return re.search(r"^[-+!]$]",s)
def klassp(s): return re.search(r"!$",s)
