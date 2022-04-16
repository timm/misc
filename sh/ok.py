
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
import re,ako,sys,tricks,warnings
the = tricks.cli(__doc__)

print(the)

def main(the):
  def header(row): 
    expects = [ako.nump(s) for s in row]
  def good(row):
    if len(expects) != len(row):
      return print( f"#E> row {n}: expected {len(expects)} cells")
    else:
      if the.strict:
        for cell,nump in zip(row,expects): 
          if not ako.good(ako.make(cell, nump), nump):
            return print(f"#E> row {n}: wrong type {cell}")
    return True
  expects=None
  for n,row in enumerate(tricks.csv()): 
    if not expects: 
      expects = [ako.nump(s) for s in row]
    else:
      if good(row): 
        print(', '.join(row))
      else:         
        the.warn -= 1
        if the.warn < 0: print(f"E> too many warnings")

if __name__ == "__main__": main(the)
