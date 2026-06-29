import fire
import random
seed=random.seed

hands=["""
    _______
---'   ____)
      (_____)
      (_____)
      (____)
---.__(___)
""",
"""
     _______
---'    ____)____
           ______)
          _______)
         _______)
---.__________)
""",
"""
    _______
---'   ____)____
          ______)
       __________)
      (____)
---.__(___)
"""]

def winner(a, b): return (a - b) % 3   # 0 tie, 1 a-wins, 2 b-wins

def three(): return random.randint(0,2)

def show(i,us,them):
  print("\033[2J\033[H", end="")
  print("\n--| "+ str(i) + " |--------------------")
  print(hands[us])
  print(hands[them])
  input(["draw\n","me!\n","oh dear\n"][winner(us,them)])

def play(repeats=200, seed=1, verbose=True):
  """Play rock-paper-scissors vs random; report wins.

  Options:
    --repeats INT   rounds to play           (default 200)
    --seed    INT   rng seed, reproducible   (default 1)
    --verbose BOOL  show hands each round    (default True; off: --noverbose)

  Examples:
    python hsr.py                       # 200 rounds, animated
    python hsr.py --repeats 1000 --noverbose
    python hsr.py --seed 42
  """
  random.seed(seed)
  win = 0
  for i in range(repeats):
    us,them = three(),three()
    if winner(us, them) == 1: win += 1
    if verbose: show(i,us, them)
  return win

fire.Fire(play)
