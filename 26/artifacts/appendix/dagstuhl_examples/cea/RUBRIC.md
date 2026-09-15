# Claim-Evidence Articulation (CEA) reading rubric  (Dagstuhl Triple-A, 15 Sep 2026)

A claim is a statement that a paper asks the reader to accept. CEA describes how clearly a claim connects to its evidence and reasoning. CEA is a working model: levels do NOT measure research quality or evidence strength.

## Four levels (cumulative; apply one claim at a time; state which evidence the review covers)
CEA1 Locate the claim: copy the exact statement; record the paper version and statement location; state why you selected it.
CEA2 Define the claim: define the properties and relationship in the claim; identify the people or things concerned; state the conditions, time period, and limits.
CEA3 Trace the evidence: connect each property to its measure and observations; show how data processing and analysis produce the result; identify the source files, versions, and result locations.
CEA4 Explain the reasoning: explain why the result supports or challenges the claim; state the assumptions, uncertainty, and limits; identify important objections and explain how the argument addresses them.
Assign the highest level for which ALL requirements are met. Record missing information. Do not fill gaps with unstated assumptions.
CEA3 does not require a successful repeat of the analysis. CEA4 does not require the reviewer to accept the argument.

## Keep separate records (per claim, outside the level)
- claim_type: describe / predict / explain; does it assert a cause? (label: descriptive | predictive | explanatory | causal; add comparative/design/negative-result if useful)
- theory: what explanation or model does the paper give?
- evidence_strength: how well does the evidence support the claim, with reasons
- file_checks: which artifact files the reviewer inspected; whether the analysis was run and produced the reported result (here: NOT run; say "not run" and list files)
- information_source: which details came from the authors, which the reviewer added, which remain unknown
A claim can reach CEA4 without a causal explanation if it does not assert a cause. A causal claim must address the assumptions needed to support that cause.

## Example
"In these projects, senior developers made fewer commits." Define "senior". Identify the developers and commit records. Show the calculation. Explain assumptions and limits. These steps give a complete evidence trace; they do not show seniority caused the difference. Flag such claims: causal_overreach = true when a descriptive/comparative observation is phrased or used as causal without addressing the causal assumptions.

## Task
Read ONE paper (pdftotext; note the version: DOI/venue/arXiv version) and its artifact package (extract to the scratchpad dir given; read README and file listing only; DO NOT run any code).
1. Extract the paper's MAJOR claims: every claim in the abstract, every numbered/bolded contribution, and the headline result of each evaluation section. Expect 4-10 claims. For each, say why you selected it.
2. For EACH claim, rate CEA1-4 per the definitions above, with evidence quotes and artifact paths. Record what is missing at the first level not met.
3. Fill the separate records.

## Output JSON to the path in your task
{"paper": "...", "paper_version": "...", "artifact_files_inspected": [...],
 "claims": [ {"id":"C1","statement":"<verbatim>","location":"<section/page>","why_selected":"...",
    "cea2":{"met":true|false,"properties_relationship":"...","subjects":"...","conditions_time_limits":"...","missing":"..."},
    "cea3":{"met":true|false,"measures":"...","analysis":"...","files_versions_results":"...","figure_or_table":"...","missing":"..."},
    "cea4":{"met":true|false,"why_supports":"...","assumptions_uncertainty_limits":"...","objections_addressed":"...","missing":"..."},
    "level":1|2|3|4,
    "claim_type":"...","asserts_cause":true|false,"theory":"...","evidence_strength":"...","file_checks":"not run; inspected: ...","information_source":"authors: ...; reviewer-added: ...; unknown: ...",
    "causal_overreach":true|false,"note":"..."} ],
 "summary":{"n_claims":N,"levels":{"1":a,"2":b,"3":c,"4":d},"causal_overreach_count":k,"time_minutes":m},
 "notes":"one line"}
Reply with only: the summary object and a per-claim list of (id, level, claim_type, causal_overreach).
