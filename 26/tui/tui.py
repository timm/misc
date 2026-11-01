#!/usr/bin/env python3
"""tui.py: a herdr-lite TUI over tab.py.

Two top-level modes -- REGRESSION and CLASSIFY -- each with its own
algorithms and its own example files. Edit the config, run, and flip
through past runs; every run shows the config it used and LEADS WITH A
LINE THAT REPRODUCES IT ON THE COMMAND LINE.

One rule: the TUI is only a dialog. Every panel comes from run(),
which batch mode calls too, so nothing here is TUI-only:

  python tui.py                               # interactive (curses)
  python tui.py --list [classify|regression]  # list example files
  python tui.py rmeans FILE.csv [bins=7 m=2 p=2]   # == Regression/rmeans
  python tui.py mean   FILE.csv                     # == Regression/mean
  python tui.py nb     FILE.csv [bins=7 m=2]        # == Classify/nb
  python tui.py zeror  FILE.csv                     # == Classify/zeroR

(c) 2026 Tim Menzies, MIT."""
import os, sys, glob, curses
from tab import Tab, Nb, Rmeans, the, thing, logfmt

DIR = os.path.expanduser("~/gits/moot")
KNOBS = ("bins", "m", "p")
MODES = ("regression", "classify")
ALGOS = {"regression": ("rmeans", "mean"), "classify": ("nb", "zeror")}

# ---- the four algorithms: each returns body lines (no reproduce head) ----
def _split(rows): n = int(.8 * len(rows)); return rows[:n], rows[n:]

# Each record is led by a keyword (config | row | repro); config nests the
# knobs, so logfmt renders them indented on their own line.
def cfg(**d): return "config " + logfmt(**d, knobs={k: the[k] for k in KNOBS})
def row(**d): return "row " + logfmt(**d)

def a_nb(t, name):
  if not t.klass: return [cfg(learner="nb", file=name, error="needs a !-klass column")]
  b = t.clone(t.rows, "bin"); tr, te = _split(b.rows); m = Nb(b, tr)
  ok = sum(m.guess(r) == r[b.klass.at] for r in te)
  return [cfg(learner="nb", file=name, klasses=m.nKlass, train=len(tr),
              test=len(te), acc=round(100 * ok / max(1, len(te)), 1))] \
         + [row(want=r[b.klass.at], got=m.guess(r)) for r in te[:8]]

def a_zeror(t, name):   # baseline: always guess the majority training klass
  if not t.klass: return [cfg(learner="zeror", file=name, error="needs a !-klass column")]
  at = t.klass.at; tr, te = _split(t.rows)
  ks = [r[at] for r in tr]; maj = max(set(ks), key=ks.count)
  ok = sum(r[at] == maj for r in te)
  return [cfg(learner="zeror", file=name, train=len(tr), test=len(te),
              guess=maj, acc=round(100 * ok / max(1, len(te)), 1))]

def a_rmeans(t, name):
  if not t.y: return [cfg(learner="rmeans", file=name, error="needs -,+ or ! goals")]
  m = Rmeans(t, t.rows[:45], t.ydist)
  return [cfg(learner="rmeans", file=name, target="ydist", mean=round(m.all.mu, 3))] \
         + [row(actual=round(t.ydist(r), 3), guess=round(m.guess(r), 3))
            for r in t.rows[:8]]

def a_mean(t, name):    # baseline: always guess the training mean ydist
  if not t.y: return [cfg(learner="mean", file=name, error="needs -,+ or ! goals")]
  tr = t.rows[:45]; mu = sum(t.ydist(r) for r in tr) / len(tr)
  return [cfg(learner="mean", file=name, target="ydist", mean=round(mu, 3))] \
         + [row(actual=round(t.ydist(r), 3), guess=round(mu, 3))
            for r in t.rows[:8]]

FN = {"nb": a_nb, "zeror": a_zeror, "rmeans": a_rmeans, "mean": a_mean}
TITLE = {"nb": "NAIVE BAYES", "zeror": "ZERO-R", "rmeans": "RMEANS", "mean": "MEAN"}

def run(algo, file):                       # logfmt lines; line 0 (repro) reruns it
  cmd = f"python tui.py {algo} {file} " + " ".join(f"{k}={the[k]}" for k in KNOBS)
  recs = [f'repro cmd="{cmd}"'] + FN[algo](Tab(file), os.path.basename(file))
  return [ln for rec in recs for ln in rec.split("\n")]   # flatten nested records

def helptext():                            # everything the TUI chrome shows
  out = ["tui.py: regression & classify over tab.py", "", "modes & algorithms:"]
  out += [f"  {m:<11}{'  '.join(ALGOS[m])}" for m in MODES]
  out += ["", "config knobs (name=value, showing current):"]
  out += [f"  {k:<6}{the[k]}" for k in KNOBS]
  out += ["", "batch (identical to the TUI's panels):",
          "  python tui.py --list [regression|classify]",
          "  python tui.py <algo> FILE.csv [bins=.. m=.. p=..]",
          "", "interactive:  python tui.py"]
  return "\n".join(out)

def datasets(mode):
  fs = glob.glob(os.path.join(DIR, "**", "*.csv"), recursive=True)
  fs = sorted(f for f in fs if not f.endswith(".orig.csv"))
  key = "/classify/" if mode == "classify" else None
  return [f for f in fs if (key and key in f) or
          (not key and ("/regression/" in f or "/optimize/" in f))] or fs

# ---- colors ----
def colors():
  curses.start_color(); curses.use_default_colors()
  for i, fg in enumerate((curses.COLOR_CYAN, curses.COLOR_GREEN,
                          curses.COLOR_YELLOW, curses.COLOR_MAGENTA,
                          curses.COLOR_WHITE), 1):
    curses.init_pair(i, fg, -1)
  curses.init_pair(6, curses.COLOR_BLACK, curses.COLOR_CYAN)   # active tab/sel
  curses.init_pair(7, curses.COLOR_WHITE, curses.COLOR_BLUE)   # title bar
C = lambda n: curses.color_pair(n)

# ---- the dialog ----
def tui(scr, _):
  curses.curs_set(0); scr.keypad(True); colors()
  mi = ai = sel = ki = 0; rail = 26; runs = []; cur = -1; show_help = False
  files = datasets(MODES[mi])

  def put(y, x, s, a=0):
    h, w = scr.getmaxyx()
    if 0 <= y < h and 0 <= x < w - 1:
      try: scr.addstr(y, x, s[:w - x - 1], a)
      except curses.error: pass

  while True:
    scr.erase(); h, w = scr.getmaxyx()
    rail = max(12, min(rail, w - 14))
    # row 0: title + mode tabs (herdr-style)
    put(0, 0, " herdr-lite ".ljust(w), C(7) | curses.A_BOLD)
    x = 13
    for i, md in enumerate(MODES):
      lab = f" {i + 1} {md.title()} "
      put(0, x, lab, (C(6) | curses.A_BOLD) if i == mi else C(7)); x += len(lab) + 1
    for y in range(1, h - 1): put(y, rail, "│", C(1))
    y = 1
    # rail top: config knobs
    put(y, 1, "config  (tab ←→)", C(1) | curses.A_BOLD); y += 1
    for j, k in enumerate(KNOBS):
      put(y, 1, f"{'>' if j == ki else ' '} {k:<5} {the[k]}",
          C(3) | curses.A_BOLD if j == ki else C(5)); y += 1
    y += 1
    # rail middle: algorithms for this mode
    put(y, 1, "algorithms  (a)", C(1) | curses.A_BOLD); y += 1
    for j, alg in enumerate(ALGOS[MODES[mi]]):
      put(y, 1, f"{'o' if j == ai else ' '} {TITLE[alg]}",
          C(6) | curses.A_BOLD if j == ai else C(5)); y += 1
    y += 1
    # rail bottom: file picker (windowed, runs to the bottom)
    put(y, 1, f"files ({sel + 1}/{len(files)})", C(1) | curses.A_BOLD); y += 1
    page = max(1, h - y - 1)
    top = max(0, min(sel - page // 2, len(files) - page))
    for row, i in enumerate(range(top, min(top + page, len(files)))):
      nm = ("o " if i == sel else "  ") + os.path.basename(files[i])
      put(y + row, 0, nm[:rail - 1],
          C(6) | curses.A_BOLD if i == sel else C(5))
    # output pane
    if show_help:
      out = helptext().split("\n")
    elif runs:
      out = runs[cur][1]
    else:
      out = ["Pick a mode (1/2), an algorithm (a), a dataset (↑↓).",
             "Edit config (tab, ←/→).  Enter runs it.",
             "[ / ] flips past runs.  , / . moves this divider.",
             "? = help (same text as `python tui.py -h`).  q = quit.",
             "", "Every run's first line reproduces it on the CLI."]
    for i, ln in enumerate(out[:h - 3]):
      a = (C(3) | curses.A_BOLD if ln.startswith("repro ") else
           C(2) | curses.A_BOLD if ln.startswith("config ") else
           C(3) if ln.startswith("  ") else                    # indented knobs
           C(5) if ln.startswith("row ") else 0)
      put(2 + i, rail + 2, ln, a)
    tag = f" run {cur + 1}/{len(runs)} " if runs else " no runs "
    put(h - 1, 0, (f" {MODES[mi].title()} · ↑↓ data · a algo · Enter run · "
                   f"[ ] flip ·{tag}· q quit").ljust(w), C(7))
    scr.refresh()

    c = scr.getch()
    if c in (ord("q"), 27): return
    elif c in (ord("1"), ord("2")):
      mi = c - ord("1"); ai = sel = 0; files = datasets(MODES[mi])
    elif c == ord("a"): ai = (ai + 1) % len(ALGOS[MODES[mi]])
    elif c in (curses.KEY_UP, ord("k")): sel = max(0, sel - 1)
    elif c in (curses.KEY_DOWN, ord("j")): sel = min(len(files) - 1, sel + 1)
    elif c == ord("\t"): ki = (ki + 1) % len(KNOBS)
    elif c in (curses.KEY_LEFT, ord("h")):
      the[KNOBS[ki]] = max(2 if ki == 0 else 0, the[KNOBS[ki]] - 1)
    elif c in (curses.KEY_RIGHT, ord("l")): the[KNOBS[ki]] += 1
    elif c == ord(","): rail -= 2
    elif c == ord("."): rail += 2
    elif c == ord("?"): show_help = not show_help
    elif c in (ord("\n"), ord(" ")):
      algo = ALGOS[MODES[mi]][ai]; show_help = False
      runs.append((algo, run(algo, files[sel]))); cur = len(runs) - 1
    elif c == ord("[") and runs: cur = (cur - 1) % len(runs)
    elif c == ord("]") and runs: cur = (cur + 1) % len(runs)

# ---- the cli ----
if __name__ == "__main__":
  a = sys.argv[1:]
  if "--dir" in a: DIR = a[a.index("--dir") + 1]
  for s in a:
    if "=" in s and s.split("=")[0] in KNOBS:
      k, v = s.split("=", 1); the[k] = thing(v)
  if a and a[0] in ("-h", "--help"):
    print(helptext())
  elif a and a[0] == "--list":
    mode = a[1] if len(a) > 1 and a[1] in MODES else "classify"
    print("\n".join(datasets(mode)))
  elif a and a[0] in FN:
    print("\n".join(run(a[0], a[1])))
  else:
    curses.wrapper(tui, None)
