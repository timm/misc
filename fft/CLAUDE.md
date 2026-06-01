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

ONE tree (random attr, supervised min-variance cut = ladder L2,
hardwired). Dropped: fastmap, `distx`, radial, `bucket`, `Bins`,
`merge`, gini, the `-L`/`sup` ladder flags.

- `Data/add/adds/Cols` — incremental stats (Welford for Num).
  NOTE `adds([])` returns `None` (lazy accumulator).
- `disty(d,r)` — distance-to-heaven over Num y-cols (Minkowski `the.p`).
- `tree(root, stop=None, bins=0)` — random x-col per node, cut at the
  VALUE minimizing child klass-variance. `split()` partitions rows AND
  accumulates each side's klass `Num` in ONE pass; impurity =
  `ly.m2+ry.m2` (m2 = n·var = gini/2 for 0/1 label). `bins=0` tries all
  values; `bins>0` only quantile cut-points (slower at bag=64, see
  Bins finding). retry ≤10 on degenerate else leaf = `Data` clone.
  Needs NUMERIC klass (0/1); multiclass strings would crash `add`.
- `treeLeaf(root,t,row)` — router (`cutgo` test). `leaves(t)` — yield
  leaf `Data`s. `treeShow(root,t)` — printer (ygoal=mean disty +
  per-goal means, best-first).
- `confused(pairs)` — `[(want,got),...]` → per-label `pd,pf,prec,acc,f1`
  (pd=recall/TPR, pf=false-alarm/FPR). Multi-class one-vs-rest.

## Tasks (import engine)

- `pick.py` — honest train/select/test (the live one). Protocol per run:
  shuffle → **60 train / 20 val / 20 test**; build `-m` models from
  class-ratio bags (x% pos, x in 10..90, **bag 64**) of train, each a
  **`tree(sub,16)`** (leaf 16, L2 cut); score each on val by
  distance-to-heaven over (recall,prec,fairness); best → test → ONE dot.
  Repeat `-r` times → `-r` test dots. Cloud chart = run #0's `-m` models.
  Emits `CLOUD` + `TEST` rows. `setVotes(t)` caches leaf majority once →
  `metrics` = lookup. Task-only here: `lab`/`grp`/`groups`, `setVotes`,
  `metrics`, `heaven`; engine stuff (`leaves`) imported from `fft1`.
- `prep.py` — raw fairness CSV → moot (keep-lists drop leaky cols; numeric
  age → `agegrp`).
- `thesis.py` — the headline figure (see command below). ARGS now
  `-m 800 -n 2000 -v 200 -r 20`.
- `ablate.py`/`valtest.py`/`ensemble.py`/`sup.py`/`levels.py`/`bins.py` —
  the studies behind the Findings (see below). Scaffolding, not live.
- `plot.py`, `grid.py`/`gridpick.py`/`land.py`/`panels.py` — figures.
- `compas.py`/`fairml.py` — older task variants.

Flags (`pick.py`): `-m` models/run (300) · `-r` reps (25) · `-n` TOTAL
rows then 60/20/20 (train=.6N) · `-v` max val rows · `-S` leaf size ·
`-g` protected col · `-f` file. (no `-L`: L2 hardwired in engine.)

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
     .06→.02, min .75→.92 as N 1000→4000. FIX = bigger N (cheap now);
     `-v` caps val so selection stays fast. `-n 4000` lifts compas/
     adult/dutch dots to top fairness band (thesis ARGS use `-n 2000`
     for speed; bump to 4000 for the tightest figure).
  2. precision pinned at base rate by label-blind splits. `sup.py`
     gini-cut lifts some (adult prec .46→.65) but trades recall; diab
     prec=base=.27, unpredictable — supervised can't help.
- Next lever for precision (color), if wanted: supervised splits.

## Supervision ladder (2026-05-31) — `levels.py` (now hardwired L2)

Ladder (was `-L`, now picked + baked into `fft1.tree`): L0 random
attr+random cut, L1 +median, L2 +best-cut, L2k K attrs+best, L3 all
attrs+best. Climbed once via `~/tmp/l{0..3}.png` (l2/l3 kept).
- **L0=L1** (label-blind, both ~h.60). **L1→L2 = the win**: label in
  cut purifies leaves, adult's low-fair scatter fixed, dots redder
  (h.586→.542). **L2→L3 = reject**: overfits 64-row bags (adult
  regresses) AND greedy best-cut collapses cloud diversity (all
  models pick same split).
- Cost (full figure): L0 227s, L2 244s (1.1×), L3 474s (2.1×).
  Supervised cut cheap because bags=64. ms/tree (bench): L0 .48, L2
  .58 (1.2×), L2k 1.47 (3.1×), L3 1.80 (3.8×).
- **Sweet spot = L2, HARDWIRED in `fft1.tree`** (no flag): one random
  attr keeps cloud spread, best (min-variance) cut adds label signal.
  diab floored regardless (precision data-property). Verified at figure
  level: simplified m2/all-values L2 == old builder png.

## Value-sampling (2026-06-01) — `bins.py`

Q: try only quantile cut-points (not all values) to go faster? A: **no
win at bag=64**. PAIRED bench (same bags, vary `bins`): bins=0 all-values
h.603; bins=5 1.4× faster but h.617 (dutch worst); bins=10/20 SLOWER
than all-values. Candidate count is NOT the cost — **tree SHAPE is**
(coarser grids build bigger trees). Helps only on BIG nodes (1200-row:
bins=5 2.1×) — irrelevant at bag=64 (nodes tiny, k→1 → all-values
anyway). ⇒ `tree` default `bins=0` (all values). Knob kept for big bags.

## Makefile

`make sh` (tuned bash), `make vi` (isolated nvim), `make ~/tmp/X.pdf`
(pretty-print src), `make push msg...` (add+commit+push+status),
`make bench`, `make check` (luacheck/ruff).
