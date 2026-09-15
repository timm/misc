# Audit rubric: re-rate one prior-work element against 13 constructs

Context: "Triple-A" is a proposed project (Feldt & Menzies) for agentic skills that bridge
paper claims and artifact evidence. Notation: P = paper, A = artifact/replication package,
L = links between claims and evidence, S = a venue standard/guideline (e.g. ACM badging,
EuroSys/USENIX artifact appendix template, SC26 AD/AE appendix), C = critique.

You are auditing ONE element (a tool/paper or small family). Read the PDF(s) fully with
`pdftotext <file> - | less` style commands (use Bash: `pdftotext FILE -`). Do not run any code.

For EACH of the 13 constructs below, output:
- rating (use the scale given),
- evidence: a verbatim quote (<= 40 words) with section/page, OR the string "no evidence found",
- confidence: high / medium / low,
- note: one sentence if the current rating is wrong or debatable.

## Use-case constructs (rate 0 = not addressed, 1 = partial, 2 = covered)
"Covered" = the tool produces that output as a deliverable for a user. "Partial" = produces a
piece of it, or only for a subclass (e.g. tables only, ML papers only), or does it as an internal
step rather than a deliverable. "Not addressed" = nothing in the paper does this.

UC1 Create an artifact appendix (venue-format document describing artifact, setup, claims->experiments) from P + A.
UC2 Adapt an existing package A1 to a target venue standard S2 (translate between standards).
UC3 Produce explicit links L from paper claims/results to artifact components (files, scripts, outputs).
UC4 Report what the PACKAGE A lacks relative to a standard S (missing files, deps, instructions...).
UC5 Report what the PAPER (or its appendix) lacks: underspecified methods, missing details, claim/artifact inconsistencies, missing validation info. (Detecting paper-artifact inconsistencies or paper underspecification counts as at least partial.)
UC6 Generate artifact material (code, environment, docs, layout) from P.
UC7 Critique a (P,A) pair by comparison with exemplar (P,A) pairs from the same domain.
UC8 Link related papers to each other.

## Bipolar design constructs (rate 1..5, 1 = left pole, 5 = right pole, 0 = not applicable)
B1 author-facing (1) <-> reviewer/evaluator-facing (5): who is the intended user?
B2 reads docs/text only (1) <-> executes code (5)
B3 starts from the paper text (1) <-> starts from the artifact/repo (5): primary input
B4 standard-agnostic (1) <-> driven by an explicit venue standard or published criteria (5)
B5 emits a score/badge/classification (1) <-> emits a document/report/appendix in prose (5)

## Output format
Write a JSON file at the path given in your task, shaped:
{"element": "...", "files": [...],
 "ratings": {"UC1": {"rating": 0, "evidence": "...", "confidence": "high", "note": ""}, ..., "B5": {...}},
 "disagreements": ["UC5: current 0, should be 1 because ..."],
 "one_line_summary": "what the tool actually does, in <= 30 words"}
Then reply with only the disagreements list and the one-line summary.
