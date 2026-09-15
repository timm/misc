# depends — what a set of papers actually reuses

Pipeline (after Baldassarre et al., CACM 2023 reuse graphs, extended to nine edge types + four flags):

1. `seeds/<name>.json` — list of seed papers: slug, title, year, cites, first_author, arxiv/doi.
2. `pdfs/<slug>.pdf` — one PDF per seed.
3. One reader agent per PDF follows `RUBRIC.md`, writes `readers/<name>/<slug>.json` (edges with type, flags, quote, confidence). No code is executed.
4. `python3 aggregate.py readers/<name> seeds/<name>.json out/<name>` → `<name>_matrix.json`, `<name>_edges.csv`. Alias merging in `canon.py`.
5. Inject matrix + notes into `template.html` (placeholders `__MATRIX__ __NOTES__ __TITLE__ __H1__ __EYEBROW__ __LEDE_INTRO__`), publish as artifact.

Edge types: steppingstone, statmethod, methodology, dataset, software, sanitycheck, numericfact, theory, replication.
Flags: self (shares an author), executed (ran it vs cited), polarity (supports/refutes/neutral), transitive (via intermediary).

Runs so far:
- `timm-fyi-16` : 16 papers from timm.fyi news, 638 edges (see ../papers/).
- `top25` : Tim Menzies's 25 most-cited works 2016–2026 per OpenAlex.

## top25 run (15 Sep 2026)
- seeds/top25.json: OpenAlex author A5077008083, 25 most-cited 2016–2026; rank 11 = arXiv copy of rank 4, read once → 24 papers.
- pdfs/: 21 from arXiv, 3 from ~/gits/timm/timm.github.io/old/pdf (HDP, Images don't lie, Five Laws, Crowds), 1 from fuwei.us (Too much automation).
- out/top25_{matrix.json,edges.csv,stats.json,html,light.png,dark.png}; artifact https://claude.ai/artifact/K72Z6NuwontTUH93LAepLi
- 735 edges, 430 targets after aliasing, 161 self, 413 executed, 89 refutes, 12 replication.
