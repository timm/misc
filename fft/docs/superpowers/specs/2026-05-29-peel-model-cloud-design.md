# Peel → (volume, disty) Model Cloud

Date: 2026-05-29
Author: Tim Menzies <timm@ieee.org>
Status: locked, pre-implementation

## Goal

Explore simple meta-learning. Hypothesis: learning is just inductive bias, and
no one bias is best for all local data + goals — so sample a wide range of cheap
models and look at the spread. Need a *fast* way to generate many models and
read off their behaviour cheaply.

This spec defines that generator: an iterative **range-peeling** algorithm built
as a minimal extension to `ranger.lua`.

## Core idea

Do NOT build rules (conjunctions need row-membership intersection = bitsets).
Instead **prune bad ranges**:

1. ranger's `weigh()` already discretizes each x-col into coarse bins, each bin a
   `Num` of y-distances (`disty`, distance-to-heaven). A single-range bin's mean
   disty is *exact* over its rows — no approximation. Trust it.
2. Flag the worst ranges (highest mean disty).
3. Score each live row by how many bad ranges it falls in (a vote).
4. Keep the bottom half by vote (rows touching fewest bad ranges).
5. Re-`weigh` the survivors, repeat. Halve until ~√n rows remain.

The surviving set is described by a few ranges over a few attributes (sparse,
auto feature-selected). The conjunction emerges by elimination, never
constructed — so no bitsets, no rule explosion.

## Key design decisions (and why)

- **Engine is label-free.** Peeling runs on continuous `disty` alone. "Worst
  range" = highest mean disty. best/rest never enters the loop.
- **best/rest is a post-hoc ruler only.** pd/pf are classification rates; they
  need a binary truth. That binarization is applied *after* peeling, as a
  swappable lens — try top 20/30/40% best without re-peeling. Sidesteps the
  "top-√N is too noisy at 50 labels" problem.
- **Native output currency = `(volume kept, mean disty)`.** Smaller volume +
  lower disty = more selective + better. This is pd/pf without binarizing; pd/pf
  is just one optional projection of it.
- **Voting, not hard-delete.** A row survives one bad-range membership if
  otherwise clean. Avoids killing good rows that share a bad range on one attr.
- **Re-weigh each round; keep `root` fixed.** Bin stats change as rows leave, so
  re-discretize on the shrinking set (the ezr recursive `(max-min)/2` spirit).
  But `disty` always uses `root`'s global col stats → stable y-scale across
  rounds. ranger's existing `root` param already does this.
- **Stop at √n leaf, not n=1.** ezr: returns flatten fast; extra depth buys
  nothing and small-n bin stats get noisy.
- **Reuse ranger's coarse bins as the one-range rules.** No reservoir, no random
  ranges in v1. Lessons: random-value cuts ≈ sorted-percentile cuts ≈ coarse
  equal-width in expectation; dumb beats clever. Reservoir/equal-frequency
  discretization is a deferred refinement.
- **Diversity comes from randomizing knobs**, not from a labelled holdout that
  grows. The 30–50 labelled rows are fixed; compute is free; spin many peels.

## Lessons from ezr baked in

- Few labels (30–50) suffice; label cost dominates, compute is free → never grow
  the label set just to score more models.
- Variance at 50 labels is real → the *cloud* of models is the deliverable, not
  one box. Meta-sampling averages out small-n noise.
- Coarse discretization is enough; can't justify finer resolution than ~√labels
  distinct cuts.
- Collapse objectives early via `disty` (already in ranger).

## Interface (≈50 lines on ranger.lua, no new struct)

```
peel(d, root, k, frac) -> {rows, vol, mu}
  live = d._rows
  loop until #live <= sqrt(#d._rows):
    dd  = Data.clone(d, live)        -- re-discretize on shrinking set
    weigh(dd, root)                  -- bins of disty, root-scaled
    bad = top-k bins by mu across all dd.cols.x      -- worst = highest disty
    for row in live:
      votes[row] = count of bad bins where dd-col:bin(row[at]) == bin-index
    live = bottom `frac` of live sorted by votes ascending
  return { rows=live, vol = #live/#d._rows, mu = dyStat(root, live).mu }

sample(d, root, N) -> cloud           -- the model generator
  for i = 1..N:
    the.bins = rand 2..8 ; k = rand 1..3 ; frac in {0.5, 0.66}
    push cloud, peel(d, root, k, frac)    -- each result = a point (vol, mu)
  return cloud

pdpf(res, best_rows) -> {pd, pf}      -- OPTIONAL post-hoc lens
  pd = #(best_rows in res.rows) / #best_rows
  pf = #(non-best in res.rows) / #non-best
```

### Reused from ranger.lua, unchanged
`disty`, `dyStat`, `Data.clone`, `weigh`, `Num` (+ `merge`), `col:bin`.

### New
`peel`, `sample`, optional `pdpf`, a small `show` for the cloud.

## Open decision

Code target: extend `ranger.lua` in place (chosen default, least code), vs. a new
`peel.lua` in the `fft` repo that `require`s ranger. Note: `ranger.lua` currently
lives in `~/gists/ranger/`, a different repo from the `fft` working dir.

## Out of scope (v1)

- Reservoir / equal-frequency / random-range discretization (deferred refinement).
- Conjunctive rule construction + bitset inverted index.
- Naive-Bayes best/rest classifier generator.
- Other cheap generators (random poles/FastMap, random linear weights,
  extra-trees cuts, random centroid) — peel is the first one to try.
