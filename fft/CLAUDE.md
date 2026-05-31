# fft — random model-cloud for fairness/performance

Hypothesis: learning = inductive bias; no one bias best for all local
data + goals. So make **wide cloud of cheap random models**, read spread
(recall, precision, fairness) not tune one model. Random coverage rivals
careful hyper-param search.

## Conventions

- **Python, no deps** but `matplotlib` for plots. Else pure stdlib.
- **Lines ≤ 65 chars.** Terse. Match surrounding style.
- **`o` = attr-dict** (`class o(dict)` in `fft1.py`): `x.a == x["a"]`,
  `**x` splat, `__repr__` = `{:k v ...}` (hides `_`-keys, rounds floats to
  `the.Round`). Structs tagged `it=` (constructor): `Num Sym Data Cols Tree`.
- **Config via docstring.** Options like `-s --seed seed=1234567891`.
  `settings(__doc__)` builds `the`; `cli(the,__doc__,globals())` parses
  argv (bool flags toggle; unknown `-x` errors; `--x` runs `test_x()`).
  Defaults live in docstring only.
- **moot CSV**: header tags cols. `Upper`-first = `Num`; lower = `Sym`;
  suffix `!`/`+`/`-` = goal (y-col); `X` = skip. `?` = missing.
- **Load data AFTER `cli()`** (in `__main__`), never module top — else
  `-f`/`-g` ignored (bug: every dataset ran as default). See `pick.py:load()`.
- Don't name a script `select.py` — shadows stdlib `select`, breaks
  matplotlib import.

## Engine — `fft1.py` (library, no task knowledge)

ONE tree (random axis-cut). Dropped: fastmap, `distx`, supervised best-cut
tree, `bucket`, `Bins`, `merge`.

- `Data/add/adds/Cols` — incremental stats (Welford for Num).
- `disty(d,r)` — distance-to-heaven over Num y-cols (Minkowski `the.p`).
- `tree(root)` — random axis-cut cluster tree: random x-col, cut at a
  random row's value (`<=v` Num / `==v` Sym), retry on `?`/degenerate
  (≤10 tries) else leaf. No `distx`, no sort. Leaf = a `Data` clone.
- `treeLeaf(root,t,row)` — router (`cutgo` test). `treeShow(root,t)` —
  printer (ygoal=mean disty + per-goal means, best-first).
- `confused(pairs)` — `[(want,got),...]` → per-label `pd,pf,prec,acc,f1`
  (pd=recall/TPR, pf=false-alarm/FPR). Multi-class one-vs-rest.

## Tasks (import engine)

- `pick.py` — honest train/select/test (the live one). Protocol per run:
  shuffle → **60 train / 20 val / 20 test**; build `-m` models from
  class-ratio bags (x% pos, x in 10..90) of train; score each on val by
  distance-to-heaven over (recall,prec,fairness); best → test → ONE dot.
  Repeat `-r` times → `-r` test dots. Cloud chart = run #0's `-m` models.
  Emits `CLOUD` + `TEST` rows. `setVotes(t)` caches leaf majority once →
  `metrics` = lookup.
- `prep.py` — raw fairness CSV → moot (keep-lists drop leaky cols; numeric
  age → `agegrp`).
- `thesis.py` — the headline figure (see command below).
- `plot.py`, `grid.py`/`gridpick.py`/`land.py`/`panels.py` — figures.
- `compas.py`/`fairml.py` — older task variants.

Flags (`pick.py`): `-m` models/run (300) · `-r` reps (25) · `-n` TOTAL
rows then 60/20/20 (train=.6N) · `-v` max val rows · `-S` leaf size ·
`-g` protected col · `-f` file.

## Commands you can just ask for

### "thesis figure for <datasets>" (headline plot)
*"thesis figure for compas adult dutch diabetes"* (any subset/order) →
build ONE combined cloud-vs-reasoning png, show it.

Run: `python3 thesis.py <keys...>`   (keys: compas adult dutch diab)
- diabetes → key `diab`; dutch → key `dutch`. No args → all four.
- writes `~/tmp/thesis_<keys-joined-by_>.png` (name from inputs). Show it.

`thesis.py` has SETS registry (file + protected attrs per key). NEW raw
dataset: `prep.py` first, add to SETS, use key. Ad-hoc: `thesis.py FILE attr1 attr2`.

Figure: rows = dataset (one each); LEFT cols = cloud (space of options,
train, run #0), RIGHT cols = heaven-selected models (result of reasoning,
held-out test); per protected attr, coloured by precision, shared scale.

## Findings (2026-05-31)

- Random axis-cut ≈ radial in quality, ~2.3× faster. Split mechanism
  barely matters; class-ratio bagging + selection do the work.
- **Zero-val (select on model's own training rows) OVERFITS** → degenerate
  top-right test picks. Held-out val necessary.
- Cost now = model BUILD (`clone(base,bag)` per model), not eval. Capping
  `-v` helps little once build dominates.
- **Leakage**: raw `COMPAS53.csv` carries `decile_score`/`is_recid`/
  `score_text` (post-outcome). `prep.py` keep-lists strip them; generic
  loaders over *all* cols leak → fake precision ~0.9.
- **fairness ≠ accuracy axis**: accuracy base-rate-pinned. Use recall-vs-pf
  (ROC) or recall-vs-fairness. fairness×recall AUC gamed by predict-all-
  positive (→1.0); ROC-AUC honest.
- thesis: `-m 1000` ≈ 10min; `-m 300` full-val ≈ 3.5min (207s).
- Paths hard-coded: `~/tmp/*` (out), `~/gits/moot/classify/*` (data).

## Ablation (2026-05-31) — `ablate.py`/`valtest.py`/`ensemble.py`/`sup.py`

Q: which diversification operators raise test dots? A: **none**.
- **All 7 operators INERT.** 4800 random-config models × 4 sets: every
  operator's values tie on mean test-rank (deltas <0.03): split
  (axis≈radial), cut (row≈mid), bag (64/128/256), leaf (4/sqrt/16),
  ratio (sweep≈half), feat (sqrt≈all), p (1≈2). 80-model smoke shows
  FAKE big deltas — small-sample noise. Per-model quality = dataset +
  draw luck, not config.
- **⇒ drop all to cheapest, free speed.** axis (6.8× vs radial), bag=64
  (5.4× vs 256), leaf=16, feat=sqrt. Applied bag64+leaf16 to `pick.py`
  → 28 vs 57 ms/model. thesis `-m 800` == old `-m 300` png (inert at
  figure level). Cheaper ≠ higher dots.
- **Selection NOT the bottleneck.** `valtest.py`: r(val-heaven,
  test-heaven)=+0.90..+0.99. best-on-val transfers to test.
- **Ensemble-vote LOSES.** `ensemble.py`: majority vote over cloud →
  base-rate preds → kills recall+fair (compas heaven 1.21 vs select
  0.60). Selection beats voting.
- **"Low dots" = 2 data ceilings, not tuning:**
  1. fairness scatter = tiny protected groups. diab test-fair sd
     .06→.02, min .75→.92 as N 1000→4000. FIX = bigger N (cheap now).
     thesis ARGS now `-m 800 -n 4000 -v 200 -r 25` (277s) → compas/
     adult/dutch dots lift to top fairness band.
  2. precision pinned at base rate by label-blind splits. `sup.py`
     gini-cut lifts some (adult prec .46→.65) but trades recall; diab
     prec=base=.27, unpredictable — supervised can't help.
- Next lever for precision (color), if wanted: supervised splits.

## Makefile

`make sh` (tuned bash), `make vi` (isolated nvim), `make ~/tmp/X.pdf`
(pretty-print src), `make push msg...` (add+commit+push+status),
`make bench`, `make check` (luacheck/ruff).
