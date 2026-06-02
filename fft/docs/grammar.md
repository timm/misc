# Grammar of the random-cloud / fastmap design space

These scripts (`tree`, `pick.py`, `fair.py`, `fmtree`, `fmpick.py`,
`ablate.py`, etc.) are all samples from one small grammar. Spelling
it out makes it obvious which knob is being twisted, and which
arrangements have not yet been tried.

## EBNF-ish

```ebnf
ALGO       ::= REPEAT { PROTOCOL }

PROTOCOL   ::= SPLIT_DATA  BUILD_CLOUD  SELECT  EVAL

SPLIT_DATA ::= "shuffle" "->" (TRAIN | TRAIN VAL | TRAIN VAL TEST)
            ;  fractions ∈ {0/0/100, 80/0/20, 60/20/20, 50/25/25}

BUILD_CLOUD ::= "for" M "in" 1..MODELS ":" BUILD_TREE

BUILD_TREE ::= BAG  TREE  LEAVES
BAG        ::= "random" k "rows from train"           ; k ∈ {16,32,64,128,all}
            |  "ratio-sweep" (10..90 % pos, fixed n)  ; class-balanced
            |  "all train"
TREE       ::= "while" leaf_size>STOP ":"
                  SPLIT_RULE  ROUTE_RULE
              "end"
SPLIT_RULE ::= "random-attr" (RANDOM_CUT | MED_CUT | BEST_CUT)
            |  "fastmap"      POLE_PICK PROJ_CUT
            |  "all-attr"     BEST_CUT
SPLIT_RULE += FEAT_SUBSPACE                            ; sqrt-cols, all
RANDOM_CUT ::= "v ~ Uniform(c.lo, c.hi)"
MED_CUT    ::= "v = median(c)"
BEST_CUT   ::= "v = argmin sum_{side}(impurity)"
            ;  impurity ∈ {gini, variance/m2, entropy, label-ratio}
            ;  candidates ∈ {all values, B quantile-edges, midpoints}
POLE_PICK  ::= "A,B = far-far over random subsample of size k"
            ;  k ∈ {2, 30, 64, all_rows}
PROJ_CUT   ::= "split at median proj(r) onto A-B line"

LEAVES     ::= (BAG | LOAD_POOL | TRAIN | none) -> "leaf.vote = mean/majority"
LOAD_POOL  ::= "sample N rows of train, route to leaves"

STOP       ::= 4 | 8 | 16 | sqrt(n) | sqrt(bag_n)
ROUTE_RULE ::= "x <= v"        (axis Num)
            |  "x == v"        (axis Sym)
            |  "proj(x) <= cut"(fastmap)

SELECT     ::= NONE                            ; first / only tree
            |  ON_TRAIN  WORTH_FN              ; intrinsic
            |  ON_VAL    WORTH_FN              ; held-out
            |  PARETO_FRONT
            |  VOTE_ENSEMBLE                   ; no select; aggregate all

WORTH_FN   ::= "heaven(recall, prec, fair)"    ; pick.py
            |  "max_{leaf}(mean d2h)"          ; fmtree minimax
            |  "mean_{leaf}(mean d2h)"
            |  "min_{leaf}(mean d2h)"          ; best-leaf only
            |  "leaf-purity weighted by size"

EVAL       ::= TEST_METRIC                     ; single best tree
            |  TEST_METRIC + AGG               ; ensemble
TEST_METRIC::= "(recall, prec, fair)"
            |  "(d2h)"
            |  "(ROC-AUC)"

GOAL_TYPE  ::= REGRESSION       (d2h over Num y-cols)
            |  CLASSIFICATION   (heaven over recall/prec/fair)

DISTANCE   ::= MINKOWSKI(p ∈ {1,2}, cols, missing-rule)
            ;  cols ∈ {x only, x+y, y only}
            ;  missing-rule = "if both ?: 1; if one ?: extreme other"
```

## Algorithms in this codebase, as instances

| name | bag | split | pole pick | leaves | select | worth |
|------|-----|-------|-----------|--------|--------|-------|
| `tree` (fft1.py)   | all rows         | random-attr + best-cut (L2) | hist `B=7`           | bag        | none | n/a |
| `pick.py`          | ratio-sweep 64   | L2 random-attr              | best                 | bag        | val  | heaven |
| `fair.py`          | all train        | fastmap                     | random poles         | bag        | threshold sweep | Pareto |
| `fmtree`           | all rows         | fastmap                     | far-far (k=30 samp.) | full recur | train | min(max leaf d2h) |
| `fmpick.py`        | 64               | fastmap                     | far-far (k=64)       | Load pool  | val  | heaven |
| `compas.py`/`ablate.py` | various      | various                     | various              | bag        | val  | heaven |

## What past sweeps tuned

- **Bag size**: 64 wins (cheap, ratio-sweep gives diversity).
- **Split mechanism**: random-attr ≈ fastmap on quality; axis 2.3× faster.
- **Cut target**: L2 best-cut wins (L0/L1 underfit, L3 overfits, collapses cloud).
- **Bins**: all-values ≈ 7 quantile-edges at bag=64 (tree shape dominates).
- **Selection**: val-pick necessary; train-self overfits.
- **Vote vs pick**: pick wins (majority vote regresses to base-rate).

## What the grammar suggests is unexplored

- Pareto-front selection (multi-objective) — only `fair.py` touches it.
- Mixed bag (ratio-sweep + fastmap poles).
- `WORTH_FN = "minimax over leaves"` applied to classification (fmtree
  style on heaven instead of d2h).
- Adaptive depth — bigger trees for harder leaves.
- Stochastic far-pole picks per recursion using `random.sample(k)`
  rather than full sort (fmtree uses k=30, fmpick uses k=64).

## Empirical relations

- Many operators are **inert** (`ablate.py`): split mechanism, cut target,
  bag size, ratio-sweep variants — almost no effect on test heaven.
  The active levers are:
  - **bag** (ratio-sweep gives cloud spread)
  - **selection** (val-pick beats train-pick and vote)
  - **data ceiling** (small protected groups cap fairness scatter)
- `fmpick` on compas (`img/l2fmap.pdf`) reproduces this: comparable to
  L2-`pick.py` heaven, marginally worse cloud shape. Confirms
  split-is-inert under the grammar.

## Reading the grammar

A new experiment = one assignment of every nonterminal to a terminal
or to a small range. Pre-register that assignment before running;
"new algorithm" usually = a new tuple from this grammar, not a new
primitive.
