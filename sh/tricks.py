# vim: ts=2 sw=2 sts=2 et :
###     ___              __        
###    /\_ \      __    /\ \       
###    \//\ \    /\_\   \ \ \____  
###      \ \ \   \/\ \   \ \ '__`\ 
###       \_\ \_  \ \ \   \ \ \L\ \
###       /\____\  \ \_\   \ \_,__/
###       \/____/   \/_/    \/___/ 

"""
python3 lib.py [OPTIONS]
Background utilities. (c) 2022, Tim Menzies

OPTIONS:
  --demo -d       run demo       = False
  --file -f  str  training data  = data/nasa93dem.csv
  --help -h       show help      = False
  --copy -C       show copyright = False
"""

import re,sys

def copyright(): print("""
This file is part of learnBOT (BOT=box of tricks): a tiny data mining workbench.
Copyright (c) 2022, Tim Menzies, timm@ieee.org

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
""")


def atom(x):
  "Coerce single words into a Python atom."
  x = x.strip()
  if x=="True" : return True
  if x=="False": return False
  try: return int(x)
  except:
    try: return float(x)
    except: return x

def atoms(lst): 
  "Coerce list of strings"
  return [atom(x) for x in lst]

class o:
  "Building and printing of structs (hiding slots starting with '_')."
  def __init__(i, **d)  : i.__dict__.update(d)
  def __repr__(i) : return "{"+ ', '.join( 
    [f":{k} {v}" for k, v in i.__dict__.items() if  k[0] != "_"])+"}"


def csv(file=None, sep=",", dull=r'([\n\t\r ]|#.*)'):
  """Iterate over lines, divided on comma. Ignore blank lines & whitespace.  
  If `file` supplied, read from that file. Else, read from standard input."""
  if file:
    with open(file) as fp:
      for s in fp: 
       s=re.sub(dull,"",s)
       if s: yield s.split(sep)
  else:
     for s in sys.stdin: 
       s=re.sub(dull,"",s)
       if s: yield s.split(sep)

def cli(s):
  """[1] Generate a dictionary of settings=values from string `s`.   
  [2] Check if the command line interface (CLI) wants to update the values.   
  [3] For boolean settings,  that flag on the CLI flips the default.   
  [4] Coerce strings to bools and numbers (if appropriate).   
  [5] If help requested, print help and exit.  
  [6] If copyright requested, print copyright and exit. """  
  d={}
  pattern = r"  (--(\S+))[\s]+(-[\S]+)[\s]+[^\n]*\s([\S]+)"
  for (want1,key,want2,x) in re.findall(pattern,s):  #... [1]
     for at,flag in enumerate(sys.argv):                   #... [2]
       if flag==want1 or flag==want2:
         x= "True"  if x=="False" else (                   #... [3]
            "False" if x=="True"  else sys.argv[at+1])
     d[key] = atom(x)                                      #... [4]
  if d.get("help", False): sys.exit(print(s))              #... [5]
  if d.get("copy", False): sys.exit(copyright())           #... [6]
  return o(**d)

if __name__ == "__main__":
  the=cli(__doc__)
  if the.demo:
    for row in csv(the.file): print(atoms(row))

