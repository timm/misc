import re,ast,sys,yaml,json,fileinput

class Nice:
  "`nice` objects can represent themselves nicely"
  __repr__= lambda i: nicely(i)

def nicely(x,short=True): 
  "convert `x` to a nested dictionary; pretty print that dictionary"
  return yaml.dump(json.loads(json.dumps(x, default=lambda i: i.__dict__)),
                   default_flow_style=True)
#-------------------------------------------------------------------------------
class Settings(Nice):
  def __init__(i,s):
    "Settings are parsed from a string"
    i._help=s
    goal = r"\n\s*-\w+\s*--(\w+).*=\s*(\S+)"
    i.__dict__.update(**{m[1]:coerce(m[2]) for m in re.finditer(goal,i._help)})
  def cli(i):
    """Settings can be updated from command line. Boolean settings need no
    argument (we just flip the default). if we see -h, print help and exit"""
    for k, v in i.__dict__.items():
      s = str(v)
      for j, x in enumerate(sys.argv):
        if ("-"+k[0]) == x or ("--"+k) == x:
          s = "True" if s == "False" else ("False" if s == "True" else sys.argv[j+1])
        i.__dict__[k] = coerce(s)
    if i.help: sys.exit(print(i._help))
    return i

def coerce(s):
  "coerce to int,float or bool"
  try:              return ast.literal_eval(s)
  except Exception: return s.strip()
#-------------------------------------------------------------------------------
def csv(file="-"):
  with  fileinput.FileInput(file) as src:
    for line in src:
      line = re.sub(r'([\n\t\r"\' ]|#.*)', '', line)
      if line: yield [coerce(x) for x in line.split(",")]
