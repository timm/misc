# Related work for Triple-A (Agentic Artifact Appendices)

Downloaded 2026-09-14 from links in Robert Feldt's two Drive reports
(guidelines+examples; LLM/agent tools review) plus 3 from a fresh web search.
34 PDFs. Not fetched: OpenART (abstract only, no paper), Veritas (repo only),
MLSys'26 deployment (blog), Nature AI Scientist (paywall), Reproscreener (ACM REP,
no open PDF found), USENIX Sec'25 full appendix proceedings (28 MB; URL in
guidelines report if wanted).

## Inventory

| file | what | Triple-A relevance |
|---|---|---|
| arxiv_2602.02235 | ArtifactCopilot (Wu et al. 2026), agent-based AE, 5 dims, D4 = Claim Support, emits "claim mapping file" | closest to UC3 (reviewer side). Claim mapping = its worst mismatch category |
| arxiv_2602.10046 | Artisan / Automated Table Reproduction (Baek & Pradel, ASE'26) | UC3 for tables only; rerunnable script per table |
| arxiv_2606.02006 | Agentic replication-package quality eval (Amougou Mbida & Angermeir, ESEM'26), 51 criteria | UC4 (S,A => missing), docs-only |
| arxiv_2603.06862 | Heye et al., LLM support for security AE (Rate/Prepare/Assess) | UC4 partial, pitfalls |
| arxiv_2606.18237 | ReproRepo, static audit vs GitHub issues | UC4 static blockers |
| arxiv_2409.11363, arxiv_2606.26158 | CORE-Bench + 2026 follow-up | benchmark for execution |
| arxiv_2601.04226 | Snelleman et al., "Problem Statement Problem": extract hypotheses/experiments/interpretations from papers | UC3's P-side structure (no A) |
| arxiv_2504.10134 | SciConv, conversational package builder | UC6 partial (env, not from paper) |
| arxiv_2412.10133 | ExecutionAgent (Bouzenia & Pradel) | env setup component |
| neurips25_repo2run | Repo2Run Dockerfile generation | env setup component |
| arxiv_2504.01848 | PaperBench | rubric-based judge of reproductions |
| arxiv_2504.17192 | Paper2Code / PaperCoder | UC6 (P => code), ML only |
| arxiv_2505.20662 | AutoReproduce | UC6 |
| arxiv_2608.24291 | ReproAgent, contract-guided paper-to-code | UC6; "contracts" = paper-derived requirements, near L |
| arxiv_2512.02812 | collaborative agents for paper reproduction | UC6 |
| arxiv_2504.20117 | ResearchCodeAgent | UC6 |
| arxiv_2507.18901 | REPRO-Bench, paper/code/result consistency (social sci) | UC3-ish assessment benchmark |
| arxiv_2602.11354 | ReplicatorBench | replication benchmark |
| kohler26_read_paper_write_code | agentic reproduction with original data, code withheld | independent check of whether P specifies A |
| arxiv_2410.05080 | ScienceAgentBench | benchmark |
| arxiv_2506.17335 | LMR-Bench | benchmark |
| emnlp25_agent_laboratory | Agent Laboratory | research-output generation |
| arxiv_2609.11728 | Barba, reproducibility in agentic age | framing |
| arxiv_2604.01072 | notebook containerization (non-LLM) | baseline |
| arxiv_2608.18398 | LEDGER claim-to-evidence trace graphs for *agent sessions* | same vocabulary, different object (agent traces, not papers) |
| arxiv_2608.18312 | artifact-centered claim-aware observability for scientific agents (Argonne) | ditto |
| arxiv_2608.09567 | Chen 2026, reproducibility audit of LLM-generated security artifacts | empirical; artifacts made by agents are bad too |
| usenixsec23_lost_at_c_appendix, usenixsec24_spf_beyond_standard_appendix, osdi22_jawa_with_appendix, asplos23_nnsmith_with_appendix | model appendices | targets for UC1 / UC7 |
| oakland19_sok_benchmarking_flaws | 22 benchmarking flaws | bad-smell source |

## Coverage matrix: Triple-A use cases vs prior work

UC1 create appendix from P+A · UC2 adapt package to venue S · UC3 P,A => L · UC4 S,A => missing · UC5 S,P,A,L => missing in paper/appendix · UC6 P => A · UC7 critique vs examples · UC8 link papers

| work | UC1 | UC2 | UC3 | UC4 | UC5 | UC6 | UC7 | UC8 |
|---|---|---|---|---|---|---|---|---|
| ArtifactCopilot | – | – | partial (internal claim-mapping file, reviewer side, ACM badge rubric only) | partial (badge rubric) | – | – | – | – |
| Artisan | – | – | partial (tables only) | – | – | – | – | – |
| Amougou Mbida & Angermeir | – | – | – | yes (51 criteria, docs-only) | – | – | – | – |
| OpenART (abstract only) | partial (structured docs from P) | – | – | yes (missing components vs derived layout) | – | partial (spec, not code) | – | – |
| Snelleman et al. | – | – | P-side only | – | – | – | – | – |
| ReproRepo / Heye | – | – | – | static blockers | – | – | – | – |
| Paper2Code, ReproAgent, AutoReproduce ... | – | – | – | – | – | yes (ML code) | – | – |
| Repo2Run, ExecutionAgent, SciConv | – | – | – | – | – | env only | – | – |
| LEDGER / Argonne observability | – | – | vocabulary only | – | – | – | – | – |

**Empty columns: UC1 (venue-format appendix generation), UC2, UC5, UC7, UC8.**
Grep evidence: "artifact appendix" appears in 0 of 30 tool/benchmark PDFs; only in the 4 example appendices.

## Verdict

Vision not done. Pieces exist:
- claim -> evidence mapping: ArtifactCopilot (reviewer side, badge-oriented, self-reported weakest dimension), Artisan (tables).
- missing-vs-standard: Amougou Mbida (51 criteria), OpenART (layout from manuscript).
- paper structure extraction: Snelleman.
Nobody: takes P + A + venue standard S and emits an author-facing appendix with claim/experiment/exclusion mapping; translates between standards; critiques vs domain exemplars.

Nearest neighbour to watch: OpenART (Muttakin, Mondal, Roy, same group as the ICSE'26 artifact study). Abstract promises manuscript-derived docs + missing-component detection. No paper yet.

## How to check further (repeatable)

1. Forward citations of ArtifactCopilot (2602.02235), Artisan (2602.10046), OpenART, Muttakin ICSE'26 (2601.02066) on Semantic Scholar / Google Scholar. New appendix-generation work will cite at least one.
2. Search strings: "artifact appendix" generation; "artifact description" LLM; "claim-to-evidence" replication package; "reproducibility appendix" agent; SC "AD/AE appendix" automation.
3. Venues to scan: ICSE/FSE/ASE artifact tracks, ESEM, ACM REP, JAWS workshop (OpenART's), ReproAI @ AAAI, sysartifacts/secartifacts organizer reports.
4. Repeat the grep here: `for f in *.pdf; do pdftotext $f - | grep -ci "artifact appendix"; done`.
