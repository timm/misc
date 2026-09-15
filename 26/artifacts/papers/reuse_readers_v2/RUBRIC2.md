# Reuse-graph reading rubric v2

You get a paper PDF and a v1 JSON of reuse edges already extracted from it (six types).
Task: (a) re-type every v1 edge under the v2 scheme, (b) add the four flags to every edge,
(c) scan the paper once more for edges v1 missed, especially replication, refutation-polarity,
numeric facts and theory. Keep the v1 quote unless a better one exists. Do not run code.
Read with `pdftotext FILE -`.

Reuse != citation. Reuse = the paper USED the thing (ran, trained on, applied, compared against,
quoted a number or theorem as a premise, adopted a definition). Background-only citations are not edges.

## v2 edge types (exactly these labels)
steppingstone : prior method used as a baseline / comparison target in experiments
statmethod    : a statistical or ranking procedure adopted from a specific prior paper (Scott-Knott, Cliff's delta, bootstrap, effect-size rules ...)
methodology   : metric, protocol, guideline, definition or named concept adopted (d2h, CK metrics, labeling protocol, Empirical Standards, "Rashomon effect", "NEO", pool-based taxonomy ...)
dataset       : a dataset or benchmark taken from prior work
software      : a tool, library, model or package executed or built upon (SMAC3, sklearn, an LLM, a fuzzer, a replication package)
numericfact   : a prior MEASUREMENT quoted as a premise ("22.5 CPU years", "500x faster", "only 5% compare to non-LLM baselines", "1,536 hours")
theory        : a formal result or bound applied (Hoeffding, Hamlet probable correctness, No-Free-Lunch, Price's law, geometric waiting time)
replication   : the paper re-runs or re-analyses a prior study's experiment or reports a prior result it regenerated
sanitycheck   : residual justification that is none of the above (why an approach is reasonable, why to avoid bad data, motivation)

Old 'sanitycheck' edges MUST be re-examined: most are numericfact or theory.

## Flags (every edge)
self      : true if the reused work has an author in common with this paper (Menzies, Ganguly, Lustosa, Senthilkumar, Rayegan, Chen (Tao/Jianfeng with Menzies), Nair, Agrawal, Peng, Yedida, Amirali, Yu ...). Judge from author names in the reference entry.
executed  : true if the paper RAN the thing in its own experiments (tool, baseline re-run, dataset loaded); false if only cited/compared-against-reported-numbers/quoted.
polarity  : "supports" | "refutes" | "neutral". refutes = the paper argues the reused claim/result is wrong, limited, or fails in its setting ("contrary to [x] we find...", "the Pareto myth", "does not hold for SE data").
transitive: true if the thing was obtained through an intermediary (e.g. a dataset taken from MOOT rather than from its origin paper) and the paper says so.

## Output JSON (write to the path in your task)
{"paper": "...", "file": "...", "title": "...",
 "edges": [ {"target": "...", "ref": "...", "doi_or_url": "...", "type": "<v2 type>",
             "self": true/false, "executed": true/false, "polarity": "supports|refutes|neutral", "transitive": true/false,
             "evidence": "<verbatim quote <= 35 words>", "section": "...", "confidence": "high|medium|low",
             "v1_type": "<the v1 type, or 'new' if added now>"} ],
 "retyped": <count of edges whose type changed>, "added": <count new>, "dropped": [<v1 targets dropped and why>],
 "notes": "<one line>"}
Reply with only: counts per v2 type, retyped/added/dropped counts, and the notes line.
