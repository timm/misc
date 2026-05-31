# fft — fastmap model-cloud for fairness/performance exploration

Meta-learning hypothesis: learning is just inductive bias, and no one
bias is best for all local data + goals. So generate a **wide cloud of
cheap random models** and read off the spread (recall, precision,
fairness) instead of carefully tuning one model. Random coverage of the
space rivals careful hyper-parameter search — see `docs/superpowers/specs/`.

## How to work in this repo (conventions)

- **Python, no deps** except `matplotlib` for plots. Pure stdlib otherwise.
- **Lines ≤ 65 chars.** Terse. Match surrounding style.
- **`o` = attribute-dict** (`class o(dict)` in `fft1.py`): `x.a == x["a"]`,
  splat with `**x`, custom `__repr__` (`{:k v ...}`, hides `_`-keys, rounds
  floats to `the.Round`). Every struct is an `o` tagged with `it=` (its
  constructor): `Num`, `Sym`, `Data`, `Cols`, `Node`.
- **Config via docstring.** Module docstring lists options as
  `-s --seed seed=1234567891`. `settings(__doc__)` builds the `the` config;
  `cli(the, __doc__)` parses `sys.argv` (bool flags toggle, unknown `-x`
  errors). Defaults live in the docstring, nowhere else.
- **CSV = "moot" format**: header row tags columns. `Upper`-first name =
  numeric (`Num`); lower = symbolic (`Sym`); suffix `!`/`+`/`-` = goal/label
  (y-col); suffix `X` = skip. `?` = missing.
- **Load data AFTER `cli()`** (inside `__main__`), never at module top —
  else `-f`/`-g` are ignored (a bug we hit: every dataset ran as the
  default). See `pick.py:load()`.
- A **model's randomness must travel with its tree**: structural choices
  (poles, cuts, bagged rows, leaf size) bake into the built tree; metric
  choices (feature subset `cols.x`, Minkowski `p`) live on the root `Data`
  so `distx`/`disty` replay them identically at test time. Don't read them
  from global `the` inside the metric.

## Engine — `fft1.py` (library, no task knowledge)

- `Data/add/adds/Cols` — incremental stats (Welford for Num).
- `distx(d,r1,r2)` — Minkowski-`p` (p,d.cols.x from `d`), min-max normed.
- `disty(d,r)` — distance-to-heaven over y-cols (multi-objective scalar).
- `rmap(root)` — fastmap tree: 2 far poles (`far` = 0.9-quantile),
  cosine projection split, recurse on row slices, **clone only at leaves**
  (leaf = a `Data`). `leaf(root,tree,row)` routes a new row down it.
- `confused(pairs)` — `[(want,got),...]` → per-label `pd,pf,prec,acc,f1`
  (pd=recall/TPR, pf=false-alarm/FPR). Multi-class via one-vs-rest.

## Tasks (import the engine)

- `compas.py` — fairness cloud on COMPAS (hard-coded FEATS, protected `-g`).
- `fairml.py` — generic: any moot CSV, label = the `!` col, protected `-g`.
- `pick.py` — honest train/select/test: 60/20/20, build cloud on fit,
  select model nearest heaven (recall,prec,fair=1) on val, report on test.
  Emits `CLOUD` (all models) + `TEST` (selected) rows.
- `prep.py` — convert raw fairness CSVs → moot format (keep-lists drop
  leaky cols; bins numeric age → `agegrp`).
- `plot.py` — stdin rows → png (recall×fairness, colored by precision;
  optional fixed `vmin vmax` for comparable plots).
- `grid.py`/`gridpick.py`/`land.py`/`panels.py` — multi-panel figures
  (datasets × protected attrs; cloud-vs-selected landscape).

## Pipelines

```
python3 compas.py | python3 plot.py ~/tmp/fair.png          # one cloud
python3 prep.py                                             # raw -> moot
python3 fairml.py -f ~/tmp/adult.csv -g gender | python3 plot.py out.png
python3 land.py ~/tmp/land.png      # cloud (left) vs selected-test (right)
```

## Gotchas

- **Leakage**: raw `COMPAS53.csv` / clean-COMPAS carry `decile_score`,
  `is_recid`, `score_text` — post-outcome. `prep.py` keep-lists strip them.
  Generic loaders that use *all* columns will leak → fake precision ~0.9.
- **fairness ≠ accuracy axis**: accuracy is base-rate-pinned; use
  recall-vs-pf (ROC) or recall-vs-fairness. fairness×recall AUC is gamed
  by predict-all-positive (→1.0); ROC-AUC is the honest number.
- Hard-coded paths: `~/tmp/*` (outputs), `~/gits/moot/classify/*` (data).
  Adjust for your machine.
- Don't name a script `select.py` — shadows stdlib `select`, breaks
  matplotlib import.

## Commands you can just ask for

### "thesis figure for <datasets>" (the headline plot)
When I say *"thesis figure for compas adult dutch diabetes"* (any subset,
any order), build ONE combined space-of-options-vs-reasoning png and show me.

Just run: `python3 thesis.py <keys...>`   (keys: compas adult dutch diab)
- diabetes -> key `diab`; dutch -> key `dutch`.
- no args  -> all four.
- writes `~/tmp/thesis_<keys-joined-by_>.png` (name from the inputs),
  e.g. `thesis_compas_adult_dutch_diab.png`. Then read/show me that file.

thesis.py has a registry (file + protected attrs per key). For a NEW raw
dataset: run `prep.py` first (leak-free keep-list, age->agegrp), add it to
SETS in thesis.py, then use its key. Ad-hoc: `thesis.py FILE attr1 attr2`.

The figure: rows = dataset x protected-attr; LEFT col = full model cloud
(space of options, train), RIGHT col = heaven-selected models (result of
reasoning, held-out test); coloured by precision, shared scale. ~15s per
dataset. Underneath: `pick.py` (60/20/20, cloud on fit, select nearest
heaven on val, report on test).

## Current state (2026-05-31) — supersedes the older spec

Engine is ONE tree now. `fft1.py`:
- `tree(root)` — **random axis-cut** cluster tree: pick a random x-col, cut
  at a random row's value (`<=v` Num / `==v` Sym), retry on `?`/degenerate
  (<=10 tries) else leaf. No `distx`, no sort. Leaf = a `Data` clone.
- `treeLeaf(root,t,row)` router (`cutgo` test), `treeShow(root,t)` printer
  (ygoal=mean disty + per-goal means, best-first), `confused`, `cli`/`the`.
- DROPPED: radial fastmap, `distx`, supervised best-cut tree, `bucket`,
  `goes`, `Bins`, Num-merge-in-`add`. ~2.3x faster than radial.

`pick.py` protocol (one run): shuffle -> **60 train / 20 val / 20 test**;
build `-m` models from class-ratio bags (x% pos, x in 10..90) of the 60%;
score each on val by distance-to-heaven over (recall, prec, fairness);
best -> apply to test -> ONE dot. Repeat `-r` times -> `-r` test dots.
Train/cloud chart = run #0's `-m` models only. Flags:
- `-m` models/run (300) · `-r` reps (25) · `-n` TOTAL rows then 60/20/20
  (train = .6N) · `-v` max val rows · `-S` leaf size · `-g` protected col.
- `setVotes(t)` caches each leaf's majority once -> `metrics` is a lookup.

FINDINGS this session:
- Random axis-cut == radial in quality, 2.3x faster. Split mechanism
  barely matters; class-ratio bagging + selection do the work.
- **Zero-val (select on the model's own training rows) OVERFITS** ->
  degenerate top-right test picks. Held-out val is necessary.
- With random-cut, the cost is now model BUILD (`clone(base,bag)` per
  model), not eval. Capping `-v` helps little once build dominates.
- thesis: `-m 1000` = 10min; `-m 300` full-val = ~3.5min (207s).

## Makefile

`make sh` (tuned bash), `make vi` (isolated nvim), `make ~/tmp/X.pdf`
(pretty-print source), `make push msg...` (add+commit+push+status),
`make bench`, `make check` (luacheck/ruff).
