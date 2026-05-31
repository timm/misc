# Fastmap Model-Cloud for Fairness/Performance Exploration

Date: 2026-05-30
Author: Tim Menzies <timm@ieee.org>
Status: built, working
Supersedes: 2026-05-29-peel-model-cloud-design.md (peel idea; we went
fastmap instead — same thesis, different generator)

## Goal

Test the meta-learning thesis: *learning is just inductive bias, and no
single bias is best for all local data + goals.* So rather than carefully
tune one model, generate a **wide cloud of cheap random models** and read
the spread of (recall, precision, fairness). Claim: cheap random coverage
of the space rivals expensive hyper-parameter optimization (cf. Cruz et
al, ICDM 2021, "Promoting Fairness through HO").

## What we built

Engine `fft1.py` (generic, no task knowledge):
- `Data/Cols/add` — incremental column stats (Welford).
- `distx` — Minkowski-`p`, min-max normed; `p` and feature-subset (`cols.x`)
  read from the `Data` so a model's metric travels with its tree.
- `rmap` — fastmap tree: two far poles (0.9-quantile), cosine-projection
  split, recurse on row slices, clone a `Data` only at the leaves.
- `leaf` — route a new row down a tree (replays poles/cuts/proj).
- `confused(pairs)` — confusion matrix from `(want,got)` → pd/pf/prec/acc/f1.

Tasks build on it: `compas.py` (COMPAS), `fairml.py` (any moot CSV),
`pick.py` (train/select/test), `prep.py` (raw→moot), plotting in
`plot.py` / `grid.py` / `land.py`.

## Key design decisions (and why)

- **Cloud, not one model.** The deliverable is the spread. One knob does
  most of the spreading: **class-ratio bagging** (train each tree on a
  random x% positives / (100-x)% negatives) — it sweeps the operating
  point along the ROC. Feature-bagging adds real model diversity (different
  distance geometry); leaf-size and Minkowski-`p` add minor diversity.
- **Unsupervised split is the ceiling.** Fastmap clusters by x-geometry,
  never the label, so leaf label-rate ≈ prevalence → precision ≈ base rate.
  The cloud *covers* recall×fairness space but does not *learn* precision
  lift. Matching strong learners (LGBM/RF) on precision needs supervised
  splits — deferred.
- **Metric randomness travels with the tree.** Feature subset + `p` live on
  the root `Data`, not global state, so test-time routing replays the exact
  training decisions. Structural randomness (poles, cuts, rows, leaf size)
  is already frozen into the built tree.
- **Honest evaluation.** `pick.py`: 60/20/20 fit/val/test. Build cloud on
  fit, select the model nearest heaven (recall=prec=fair=1) on val, report
  on test. The selected models generalize (small val→test gap).
- **Leakage discipline.** `prep.py` keep-lists strip post-outcome columns
  (COMPAS `decile_score`/`is_recid`/`score_text`). Feeding all columns
  fakes precision ~0.9.

## Findings

- The random cloud blankets the full recall×fairness space on every
  dataset/protected-attr — the thesis holds for *coverage*.
- ~120 cheap models reach ~95% of the achievable ROC-AUC ceiling in ~2s
  (game.py ablation). One real knob (class-ratio) does most of the work.
- **Precision is a dataset property**, not knob-tunable: dutch > compas >
  adult ≈ diab (last two below base rate). Same model, data sets the ceiling.
- **Fairness is attribute-dependent**: same predictions, very different
  fairness on race vs sex vs age (e.g. COMPAS age easy, adult race hard).
- Metrics matter: accuracy is base-rate-pinned (useless axis here);
  fairness×recall AUC is gamed by predict-all-positive (→1.0). Use ROC-AUC
  (recall vs pf) as the honest scalar.

## Datasets (moot format, via prep.py)

COMPAS (race/sex/age_cat), Adult (race/gender/age), Dutch census
(sex/country_birth/age), Diabetes (race/gender/age). Sources: UCI +
Le Quy et al survey repo (tailequy/fairness_dataset). KDD census dropped
— 126 MB, too slow to parse repeatedly.

## Deliverable

`land.py` → one landscape PNG: LEFT = model cloud (space of options,
train), RIGHT = selected models (result of reasoning, held-out test),
rows = datasets, 3 protected attrs each, colored by precision.

## Out of scope (next)

- Supervised splits (label-driven pole/cut) to get precision lift.
- Loading huge datasets once and reusing across protected attrs.
- Comparison row against Cruz Table IV tuner numbers (needs their appendix).
