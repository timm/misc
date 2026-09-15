# depends reading rubric (v2 types, four flags, fresh read)

Task: for ONE paper, list every prior work it actually REUSES, typed and flagged.
Read with `pdftotext FILE -` (Bash). Do not run any code.

Reuse != citation. Reuse = the paper USED the thing: ran the tool, trained on the dataset,
applied the statistical test, adopted the metric/protocol/definition, benchmarked against the
method, quoted a prior measurement or theorem as a premise, or re-ran a prior experiment.
Background-only citations ("see also [12]") are NOT edges. When unsure, keep the edge and
mark confidence low.

## Edge types (exactly these labels)
steppingstone : prior method used as a baseline / comparison target in experiments
statmethod    : a statistical or ranking procedure adopted from a specific prior paper (Scott-Knott, Cliff's delta, bootstrap, effect-size rules, Nemenyi ...). NOT textbook t-tests with no citation.
methodology   : metric, protocol, guideline, definition or named concept adopted (CK metrics, d2h, labeling protocol, Empirical Standards, "Rashomon effect", pool-based taxonomy ...)
dataset       : a dataset or benchmark taken from prior work (name it; give origin ref)
software      : a tool, library, model or package executed or built upon (SMAC3, sklearn, an LLM, a fuzzer, a replication package)
numericfact   : a prior MEASUREMENT quoted as a premise ("22.5 CPU years", "500x faster", "only 5% compare to non-LLM baselines")
theory        : a formal result or bound applied (Hoeffding, probable correctness, No-Free-Lunch, Price's law)
replication   : the paper re-runs or re-analyses a prior study's experiment, or regenerates a prior result
sanitycheck   : residual justification that is none of the above (why an approach is reasonable, why to avoid bad data, motivation)

## Flags (every edge)
self      : true if the reused work shares an author with this paper. Judge from author names in the reference entry.
executed  : true if the paper RAN the thing in its own experiments; false if only cited / compared against reported numbers / quoted.
polarity  : "supports" | "refutes" | "neutral". refutes = the paper argues the reused claim/result is wrong, limited, or fails in its setting.
transitive: true if obtained through an intermediary (e.g. a dataset taken from a repository rather than its origin paper) and the paper says so.

## Output JSON (write to the path in your task)
{"paper": "<slug>", "file": "...", "title": "...",
 "edges": [ {"target": "<short human name, e.g. 'SMAC (Hutter et al.)' or 'PROMISE NASA defect data'>",
             "ref": "<reference entry as printed, <= 150 chars>", "doi_or_url": "<if given, else ''>",
             "type": "<type>", "self": true/false, "executed": true/false, "polarity": "...", "transitive": true/false,
             "evidence": "<verbatim quote <= 35 words showing the USE>", "section": "<where>", "confidence": "high|medium|low"} ],
 "not_reuse_examples": ["<2-3 citations deliberately excluded and why>"],
 "notes": "<one line: paper kind, anything odd>"}
Aim for completeness on steppingstone, dataset, software, replication, refutes-polarity; be selective on sanitycheck.
Reply with only: edge count per type, flag counts, and the notes line.
