
# vim: ts=2 sw=2 sts=2 et :
###             __                                            
###            /\ \                                           
###      ___   \ \ \/'\               _____    __  __    
###     / __`\  \ \ , <              /\ '__`\ /\ \/\ \   
###    /\ \L\ \  \ \ \\`\         __ \ \ \L\ \\ \ \_\ \  
###    \ \____/   \ \_\ \_\      /\_\ \ \ ,__/ \/`____ \ 
###     \/___/     \/_/\/_/      \/_/  \ \ \/   `/___/> \
###                                     \ \_\      /\___/
###                                      \/_/      \/__/ 

"""
cat csvFile | python3 ok.py [OPTIONS]
check if a csv file is ok

OPTIONS:
  --stroct -s     use row1 to define checks for other rows = False
  --help -h       show help      = False
  --copy -C       show copyright  = False
"""
import re,ako,sys,lib

def ok(n,row, expects=None):
  if expects:
    assert(len(expect)==len(row), 
           f"Error row {n}: expected #(len(expect) cells")
expects=None
    for cellzip(expects,row)

if __name__ == "__main__":
  the=cli(__doc__)
  for n,row in enumerate(csv(the.file)): ok(n,row)

