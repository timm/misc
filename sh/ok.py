
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
Check csv rows are right size. Optionally, check if cells are of right type.
Print rows that pass all tasks. After more than `-q` problems, exit

OPTIONS:
  --strict -s     use row1 to define checks for other rows = False
  --warn  -w int  number of warnings before quitting       = 20
  --help -h       show help                                = False
  --copy -C       show copyright                           = False
"""
import re,ako,sys,lib,warnings

def main():
  expects=None
  for n,row in enumerate(csv(the.file)): 
    if not expects:
      expects = [ako.nump(s) for s in row]
    else:
      oops=False
      if  len(expect)!=len(row):
         warnings.warn( f"Error row {n}: expected #(len(expect) cells")
         the.warn -= 1
         oops=True
      else:
         if the.strict:
           for cell,nump in zip(row,expects): 
             if not good(make(cell, nump), nump):
               warnings.warn( f"Error row {n}: wrong type {cell}")
               the.warn -= 1
               oops=True
      assert(the.warn > 0, "too many warnings")
      if not oops:
        print(', '.join(row))

if __name__ == "__main__":
  the=cli(__doc__)
  main()
 
