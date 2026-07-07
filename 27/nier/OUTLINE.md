# Outline: "Easier AI" NIER paper

Working title options:
- Easier AI: If Anyone Can Do It for a Dollar, Can You Do It for a Cent?
- Less, But Better: The Case for Easier AI in Software Engineering
- Easier AI for SE: Why, How, and Why Not Yet

Page budget: 4 pages main text + 1 page references. Roughly half a page
per section below, except Sections II and III which get about a page each.

## I. Introduction (~0.5 page)

Lehman's second law: complexity grows unless work is done to reduce it.
The claim: much of AI-for-SE can be radically simpler, and the community
is not doing that work. The tease: 50 labels, trees with 4-9 attributes,
85% of best, on one laptop in two hours. Contributions: the argument,
the EZR/MOOT evidence, and a research agenda (Future Plans).

## II. Why Easier AI? (~1 page)

Economics: Wirth's plea, Jevons paradox, Bain's $800B/yr revenue gap,
capex $725B (2026) heading past $1T. Trust: small models can be watched,
audited, fixed. Demand-side domains: Green AI, the global south,
resource-constrained teaching, privacy-constrained and air-gapped
industry, edge inference. SE's own lessons: reuse as attack surface
(left-pad, MOVEit, Falcon, ByBit; ~$24B in 2025), survivability (SQLite
on Mars), maintainability, option overload (Xu et al.). Data ceiling:
big AI is running out of human-generated data. And more was never always
better: SVM+TFIDF beats deep learners 100x faster; trees beat deep nets
on tabular data.

## III. How: Active Learning with EZR (~1 page, incl. 1-2 figures/tables)

The long history of "less": Ockham 1300, PCA 1902, prototypes 1974,
active learning 2009, distillation 2020s. Focus on active learning:
use the model so far to pick the next label (explore/exploit, cf. SMAC3).
Evaluation rig: 127 MOOT datasets from recent SE papers (3-1044 x
attributes, 1-8 y goals, 93-100,000 rows); win = 1 - normalized regret.
Results: label 50, check 5, reach 85% of best; 100,000 runs, one laptop,
two hours. Speed: what NSGA-II needs 1000 samples for, EZR reaches in 50;
500x less CPU means 500x less energy (the CFP's sustainability angle,
one sentence). Explanation: trees use 4-9 of up to 1044 attributes,
competitive with SOTA attribute selectors.

Candidate figures: the win-annotated tree (slide 16); the
EZR-vs-other-tools grid (slide 20).

## IV. Why Not More Easier AI? (~0.75 page)

Weak empirical standards: of 229 SE-LLM papers, only 5% baselined against
simpler methods; that is a methodological error. Incentives: "big" sells;
data centers get ribbon-cuttings, hundred-line scripts do not. The
simplicity paradox: certifying "simple is enough" costs CPU and needs
experience. Cognitive bias: humans prefer additive over subtractive
changes about 4:1 (Adams et al., Nature 2021).

## V. Future Plans (~0.5 page, required section, must have this exact title)

Turn the argument into community infrastructure: grow MOOT; establish
"compared to what simpler?" as a review norm; the data-light challenge
(when do a handful of labels suffice, and when do they fail?); hybrid
directions (LLM warm-starts for active learning; classical-then-LLM
ordering); teaching easier-AI-first; the ASE'26 workshop as first venue.
Concrete plan toward a full-length paper: characterize which task
features predict when simple fails.

## VI. Conclusion (~0.25 page)

Back to Lehman: reducing AI's complexity is work, and it is our work.
Do: baseline the simple (pip install ezr, tiny.cc/moot). Review: ask
"compared to what simpler?". Teach: easier AI first, scale only when
simple fails.

## References (1 page, hard cap)

Slides carry 37 refs. Budget ~25-30 after merging. All self-citations in
third person (double-anonymous).

## Notes

- Double-anonymous: author line and acknowledgements (students from the
  supervision-tree slide: Ganguly, Rayegan, Srinivasan, Agrawal, Nair,
  Chen, Lustosa) go in camera-ready only.
- NIER wants forward-looking, not a compressed research paper: Sections
  IV and V carry the "new idea" weight; Section III is the "emerging
  results" evidence.
