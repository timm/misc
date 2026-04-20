# References

Working bibliography for *Small AI, by Design*. Organized by the theme each
reference lands in. **TBD** = citation to pin down.

## Prologue — Arc 1: the history of less

- **Gödel, K.** (1931). *Über formal unentscheidbare Sätze...* Step 2 — rationality
  becomes an empirical study.
- **Kirkpatrick, S., Gelatt, C. D., Vecchi, M. P.** (1983). *Optimization by
  Simulated Annealing.* Science 220(4598). Step 3 — sampling → convergence.
- **Holland, J. H.** (1975). *Adaptation in Natural and Artificial Systems.*
  Step 4 — GAs as population-based SA.
- **Selman, B., Levesque, H., Mitchell, D.** (1992). *A New Method for Solving
  Hard Satisfiability Problems* (GSAT). AAAI-92. Step 6 — the birth of
  random-restart local search as a named strategy.
- **Luby, M., Sinclair, A., Zuckerman, D.** (1993). *Optimal Speedup of Las
  Vegas Algorithms.* Info. Proc. Letters 47. Theory of optimal restart
  schedules — pairs with Selman '92.
- **Domingos, P.** (1997). *Why Does Bagging Work? A Bayesian Account.*
  https://gwern.net/doc/ai/1997-domingos.pdf  Step 9 — surrogates don't need to
  be accurate.
- **Nair, V., Menzies, T., et al.** (2017). *Using Bad Learners to Find Good
  Configurations.* arXiv:1702.05701. https://arxiv.org/pdf/1702.05701  Step 9
  confirmation — poor-accuracy surrogates rank correctly.
- **Sutton, R.** (2019). *The Bitter Lesson.*
  http://www.incompleteideas.net/IncIdeas/BitterLesson.html  Used as foil: we
  accept rule 1 (hand-coded priors fail), reject rule 2 (therefore scale
  compute).

## Theme I — The landscape nobody looks at

- **Tukey, J. W.** (1977). *Exploratory Data Analysis.* Addison-Wesley. The
  original "look at the data first" manifesto.
- **Welford, B. P.** (1962). *Note on a Method for Calculating Corrected Sums
  of Squares and Products.* Technometrics 4(3). One-pass variance — the
  `NUM._add` update.
- **Kohavi, R. & John, G. H.** (1997). *Wrappers for Feature Subset Selection.*
  Artificial Intelligence 97(1-2). N attributes → M ≪ N still works.
- **Williams, R., Gomes, C., Selman, B.** (2003). *Backdoors to Typical Case
  Complexity.* IJCAI-03. Large systems often have small controller sets.
- **Johnson, W. B. & Lindenstrauss, J.** (1984). *Extensions of Lipschitz
  Mappings into a Hilbert Space.* High-dim data approximable in low-dim with
  bounded distortion.
- **Tenenbaum, J. B., de Silva, V., Langford, J. C.** (2000). *A Global
  Geometric Framework for Nonlinear Dimensionality Reduction* (Isomap).
  Science 290.
- **Roweis, S. T. & Saul, L. K.** (2000). *Nonlinear Dimensionality Reduction
  by Locally Linear Embedding* (LLE). Science 290.
- **Belkin, M. & Niyogi, P.** (2003). *Laplacian Eigenmaps for Dimensionality
  Reduction and Data Representation.* Neural Computation 15(6). Manifold / SSL
  foundations (~2000–2005 wave).
- **Chapelle, O., Schölkopf, B., Zien, A.** (2006). *Semi-Supervised Learning*
  (book). MIT Press. Consolidates the manifold/SSL wave.

## Theme II — Causation is easy (Pearl's three-rung ladder)

- **Pearl, J. & Mackenzie, D.** (2018). *The Book of Why.* Basic Books.
  Association → intervention → counterfactual.
- **Wachter, S., Mittelstadt, B., Russell, C.** (2017). *Counterfactual
  Explanations Without Opening the Black Box.* Harvard JOLT 31(2). Canonical
  XAI-via-counterfactual paper.

## Theme III — Under-the-hood sameness

- **Buse, R. P. L. & Zimmermann, T.** (2012). *Information Needs for Software
  Development Analytics.* ICSE 2012.
  https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/buse-icse-2012.pdf
  Fig. 6 — a BI tool/task matrix. Both axes collapse to Pearl's three rungs
  → three tree traversals.

## Theme IV — You don't need much data

- **Domingos, P.** (1997) — see prologue. Bad predictor → good optimizer.
- **Nair et al.** (2017) — see prologue. Confirmation.
- *(Active-learning lineage refs TBD: tie in ezr family.)*

## Theme V — Your leaderboard is mostly noise

- **Cliff, N.** (1993). *Dominance Statistics: Ordinal Analyses to Answer
  Ordinal Questions.* Psych. Bulletin 114(3). Cliff's Delta.
- **Kolmogorov–Smirnov** two-sample test.
- **Arcuri, A. & Briand, L.** (2011). *A Practical Guide for Using Statistical
  Tests to Assess Randomized Algorithms in Software Engineering.* ICSE 2011.
  TBD: exact venue/page.

## Theme VI — Deltas are simpler than the things

- **Menzies, T. & Hu, Y.** (2003). *Data Mining for Very Busy People.* IEEE
  Computer 36(11). TAR2 — treatment learning.
- **Bay, S. D. & Pazzani, M. J.** (2001). *Detecting Group Differences: Mining
  Contrast Sets.* Data Mining and Knowledge Discovery 5(3). STUCCO — the
  canonical contrast-set mining paper.
- **TBD:** precise WHICH citation — the `b²/(b+r)` score + rank-weighted
  mating scheme (Menzies lineage).

## Theme VII — Agents yes; LLMs all the way down, no

- **TBD:** ReAct (Yao et al., ICLR 2023), Toolformer (Schick et al., 2023),
  and other LLM-as-router + tool-worker architectures.

## Sidebars / craft

- **Restarts dominate mutation policy** — finding in `ezeg.py` `test_compare`.
  Connects to Selman (GSAT) + Luby (1993) in the prologue.
- **Sigmoid-of-z as Gaussian-CDF approximation** — `NUM.norm` trick.
  TBD: standard stat-tables ref, or Bowling et al. (2009) logistic CDF
  approximation.
- **Minkowski distance** — standard reference; `disty`/`distx` shared `p`.

## ezr lineage (author's prior work to link)

- **TBD:** one-line annotations for: SWAY, FLASH, HYPER, DUO, ePAL, iSBSE.
  Each with "referenced in chapter X" pointer.
