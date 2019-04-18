# farcade pattern
class ok:
  tries,fails = 0,0  #  tracks the record so far
  def __init__(i,fun=None):
    def score(t, f): 
      return f"# PASS= {t-f} FAIL= {f} %PASS = {(t-f)/(t+0.0001):.0%}"
    if not fun:     
      return print(score(ok.tries, ok.fails))
    try:
      ok.tries += 1
      print("### ",fun.__name__)
      fun()
    except Exception:
      ok.fails += 1
      print(ok.fails,ok.tries)
      import traceback
      print(traceback.format_exc())
      print(score(ok.tries, ok.fails),':',fun.__name__)

@ok
def ok1():
  "always no. tests the test engine"
  assert 1==2,'well that is a complete surprise'

@ok
def ok2():
  x=4/2
  assert x==2

ok()
