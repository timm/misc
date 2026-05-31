 Done — fast single-pole (rmapf/leaff) is now the default; -P opts into 2-pole projection.
  landfaster.py deleted (land.py is fast now).

  Sources of variance in the current build — three per-model knobs + one across-rep:

  1. Class-ratio bag (x) — each tree trains on a random 10–90% positives / rest negatives. This
  is the big one — it slides the operating point along the ROC (low x →
  conservative/low-recall, high x → liberal/high-recall). Most of the recall spread comes from
  here.
  2. Row bag (128) — each tree gets a random 128-row sample of the fit set. Bootstrap-style
  diversity.
  3. Random pole — rmapf picks one random point per node to split on. Split-geometry noise.
  (Was 2 far poles + projection before; barely mattered, hence the swap.)

  Plus, across the reps shuffles: each rep re-splits 60/20/20, so train/val/test composition
  varies → between-rep spread.

  What's fixed (no longer varied — we found they didn't help the cloud): leaf size (√n),
  Minkowski p (=2), all features (no feature-bag), leaf-majority vote (no random
  threshold/kNN). Those were diversity experiments that the ablation showed were swamped by #1.

  On model count: a cell = trees × reps models (default 60 × 12 = 720 cloud points, 12
  selected). The 512 you remember was an earlier single-split run with trees=512 — same idea,
  just all variance from knobs 1–3 in one split rather than spread across reps.

