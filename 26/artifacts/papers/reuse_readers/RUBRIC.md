# Reuse-graph reading rubric (after Baldassarre et al., CACM 2023, "(Re)Use of Research Results (Is Rampant)")

Goal: for ONE paper, list every prior work it actually REUSES, typed. Reuse != citation.
A citation is reuse only if the paper USED the thing: ran the tool, trained on the dataset,
applied the statistical test, adopted the metric or protocol, benchmarked against the method,
or followed its guidance to justify a design choice. "See also [12]" or background-only
citations are NOT reuse. When unsure, mark confidence low rather than dropping the edge.

Read the paper with `pdftotext FILE -` (Bash). Do not run any code.

## Six edge types (use exactly these labels)
1. steppingstone  : prior state-of-the-art method used as a baseline / comparison target in experiments.
2. statmethod     : a recent statistical or analysis method from the literature (Scott-Knott, Cliff's delta, effect-size rules, bootstrap procedures, Arcuri & Briand guidance, etc.). NOT textbook t-tests.
3. methodology    : metrics, protocols, guidelines, research methods adopted from a specific prior paper (e.g. CK metrics, distance-to-heaven, a labeling protocol, an empirical standard, a survey instrument).
4. dataset        : a dataset or benchmark collection taken from prior work (name it; give its origin paper/URL if the paper does).
5. sanitycheck    : a prior result cited to justify why an approach is reasonable or why to avoid bad data (e.g. "we exclude X because [ref] showed it is unreliable").
6. software       : a tool, library, model or artifact executed or built upon (e.g. SMAC, DEHB, scikit-learn, an LLM, a fuzzer, another paper's replication package).

## Output
Write JSON to the path given in your task:
{"paper": "<short name>", "file": "...", "title": "...",
 "edges": [
   {"target": "<short human name, e.g. 'MOOT repository' or 'SMAC (Hutter et al.)'>",
    "ref": "<the reference entry as printed, or URL, <= 150 chars>",
    "doi_or_url": "<if given in the refs, else ''>",
    "type": "steppingstone|statmethod|methodology|dataset|sanitycheck|software",
    "evidence": "<verbatim quote <= 35 words showing the USE>",
    "section": "<where>",
    "confidence": "high|medium|low"}
 ],
 "not_reuse_examples": ["<2-3 citations you deliberately excluded and why, one line each>"],
 "notes": "<one line: anything odd (position paper, no experiments, etc.)>"}
Aim for completeness on types 1, 4 and 6 (they are easiest to verify); be selective on 5.
Reply with only: edge count per type, and the notes line.
