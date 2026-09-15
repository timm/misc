# Audit of the Triple-A repertory grid (14 Sep 2026)

Every cell re-rated by an independent read of the source PDF (one subagent per element, rubric in RUBRIC.md, quote required). 31 of 156 cells changed.

## Changed cells

| element | construct | before | after | reason |
|---|---|---|---|---|
| vision | B2 | 2 | 3 | was 2; doc is explicitly undecided, so midpoint 3 is honest |
| artifactcopilot | UC4 | 1 | 2 | Report lists violated checklist items per dimension plus severity-ranked flags relative to an explicit ACM-grounded rubr |
| artifactcopilot | B4 | 4 | 5 | Whole system is driven by an explicit published rubric (ArtifactGuide) grounded in ACM badging policy and ICSE/ASE/FSE/I |
| artisan | UC5 | 0 | 1 | Current 0 is wrong: the rubric says detecting paper-artifact inconsistencies counts as at least partial, and this is an  |
| artisan | UC6 | 0 | 1 | Debatable: Artisan generates new artifact code (a reproduction script, sometimes fixing broken artifact code) but requir |
| artisan | B1 | 4 | 3 | Current 4 is slightly high: the paper names four audiences with authors listed first and evaluators second, though the k |
| amougou | B3 | 5 | 4 | Pipeline entry point is the paper PDF (used to find the artifact and as RAG context), but all 31 operationalized criteri |
| openart | UC5 | 0 | 1 | Current 0; rubric says detecting paper-artifact inconsistencies counts as at least partial, and 'inconsistent' here is r |
| openart | B4 | 2 | 1 | Current 2; the spec is derived from each manuscript, and no venue standard or published criteria drives the tool, so 1 f |
| snelleman | B1 | 3 | 4 | Intended consumer is an independent reproducer / automated reproducibility agent, not the original author; authors only  |
| reprorepo | UC3 | 0 | 1 | Current 0 is debatable: each finding must cite repo file paths/line refs and the paper claims or metrics it affects, i.e |
| reprorepo | UC5 | 0 | 1 | Current 0 is wrong: the agent is explicitly asked to surface paper/repository mismatches and result/metric provenance ga |
| reprorepo | B3 | 3 | 4 | Current 3 is debatable: inputs are paper+repo, but findings are grounded in repository evidence (path, line_refs) and th |
| reprorepo | B5 | 2 | 3 | Current 2 is debatable: the framework emits match-rate scores, but the audit agent's deliverable is a ranked JSON report |
| heye | UC5 | 0 | 1 | ASSESS flags methodological gaps in the paper text (e.g. lab-only evaluation, inappropriate baseline/threat model from A |
| heye | UC6 | 0 | 1 | PREPARE autonomously generates an execution environment (Docker container) as a deliverable, but from A+Readme+P rather  |
| heye | B4 | 2 | 3 | ASSESS is explicitly driven by a published pitfall taxonomy and RATE is trained against Olszewski's reproducibility labe |
| paper2code | UC3 | 0 | 1 | Current 0 is wrong for the family: ReproAgent builds paper-snippet->unit->file provenance and exports it as audit record |
| paper2code | UC5 | 0 | 1 | Current 0 is debatable: PaperCoder's planning prompt has an 'Anything UNCLEAR ... clarifications needed from the paper'  |
| paper2code | UC8 | 0 | 1 | Current 0 is wrong for the family: AutoReproduce's Paper Lineage and ReproAgent's reference-evidence channel ('related r |
| paper2code | B2 | 5 | 4 | Current 5 is debatable at family level: ReproAgent and AutoReproduce (Docker, sampled-batch unit tests, up to 20 debug t |
| repo2run | B1 | 2 | 3 | Current 2 is debatable: the intended user is a third party (benchmark builder, dataset curator, contributor) running an  |
| benchmarks | UC3 | 0 | 1 | PaperBench's judge maps each rubric leaf (a paper claim/result/method requirement) to specific submission files and logs |
| benchmarks | UC5 | 0 | 1 | Partial only by the rubric's letter: PaperBench documents paper underspecification (addendums, App. B gpt-4o 'Reproducib |
| benchmarks | UC6 | 0 | 2 | PaperBench's whole task is generating code + environment/run script + README from P alone (author code blacklisted); a s |
| benchmarks | B3 | 4 | 3 | Bimodal, not 4: CORE-Bench is 5 (agent gets capsule/README/Dockerfile and never the paper), PaperBench is 1 (agent gets  |
| benchmarks | B4 | 1 | 3 | Split: CORE-Bench is ~1 (success = values within a 95% prediction interval of manual runs, Fig. 4, p.7; no external stan |
| ledger | UC3 | 0 | 1 | Both produce claim->artifact links as the deliverable but only for agent-generated claims (LEDGER: session conclusions,  |
| ledger | UC5 | 0 | 1 | Argonne's whole worked example (Sec 6, Table 2, App. C 'Hallucinated number') is detecting claim/evidence mismatch in a  |
| ledger | B1 | 5 | 4 | LEDGER's 'reviewer' is the agent operator auditing their own session (~3-4); Argonne targets external 'scientists, revie |
| ledger | B4 | 1 | 2 | LEDGER is fully agnostic (1); Argonne proposes its own profile, aligns to PROV-O/RO-Crate/OpenTelemetry, and pitches ven |

## Claims in the earlier notes, checked

| claim | verdict |
|---|---|
| UC1, UC2, UC5, UC7, UC8 have no prior-work coverage | **Wrong for UC5** (7 partials) and UC8 (1 partial: AutoReproduce paper lineage). UC1 has 1 partial (OpenART). Holds for UC2, UC7. |
| UC3 has partials only, ArtifactCopilot strongest | Partials only still true, but 7 not 3; ReproRepo, ReproAgent, PaperBench judge, LEDGER all produce claim-to-file links as internal steps. |
| ArtifactCopilot reports claim mapping as its weakest dimension | **Overstated.** Paper says claim mapping and reuse transfer are the dominant *mismatch categories vs human reviewers*, not its weakest dimension score. |
| OpenART shares two cells with the vision | Now three (UC1, UC4, UC5 partial, UC6 partial). Abstract only, low confidence. Same authors as the ICSE'26 study: confirmed. |
| Amougou Mbida owns UC4 with 51 criteria, docs-only | Criteria count: 51 consolidated, **31 operationalized** in the tool. Docs-only confirmed. ArtifactCopilot also rated 2 on UC4 after audit. |
| Prior work sits reviewer-side, executes, emits badges | **Too strong.** Author-facing or neutral: OpenART, Amougou Mbida, Artisan (authors listed first), Paper2Code. Docs-only: Amougou, OpenART, Snelleman, ReproRepo. Document-emitting: OpenART, Snelleman, ReproRepo. |
| Vision + Snelleman form their own family | **Changed.** Audited clustering pairs the vision with OpenART, then ArtifactCopilot and Amougou Mbida. Snelleman moved to reviewer-side (B1 4). |
| Vision undecided only on docs vs executes | Holds; B2 moved to midpoint 3 to reflect that. |

## Per-cell evidence

### Triple-A vision

- **UC1** = 2 (high): '1. Create an artifact appendix. Input: paper draft and existing artifacts. Output: an artifact appendix' (Initial use cases)
- **UC2** = 2 (high): 'P, A1, [S1], S2 => A2 + adaptation report' (use case 2)
- **UC3** = 2 (high): '3. Establish claim-evidence links. P, A => L' (use case 3)
- **UC4** = 2 (high): 'S, [P], A => what is missing in A' (use case 4)
- **UC5** = 2 (medium): 'S, P, A, L => what is missing in the paper or appendix ... its exact scope remains open' (use case 5)
- **UC6** = 2 (medium): '6. Create an artifact from a paper. P => A ... that boundary needs definition'
- **UC7** = 2 (high): '(P, A, L)1 + comparison examples (P, A, L)2..n => C'
- **UC8** = 1 (high): 'Retain this as an exploratory use case. The notes do not specify...'
- **B1** = 2 (high): 'Supporting you in creating an Artifact Appendix from your paper draft and artifacts'; 'Help researchers make the relationship ... explicit'
- **B2** = 3 (high): 'Should the first checking skill inspect documentation only, or also execute artifacts?' (Questions to resolve next)
- **B3** = 3 (high): 'Input: paper draft and existing artifacts'
- **B4** = 5 (high): 'S = Standards or guidelines, including venue expectations'; use cases 2, 4, 5 take S as input
- **B5** = 5 (high): 'the useful deliverable is therefore an appendix draft plus a claim-by-claim coverage and missing-information report' (guidelines report, synthesis section)

### ArtifactCopilot

- **UC1** = 0 (high): no evidence found
- **UC2** = 0 (high): no evidence found
- **UC3** = 1 (high): a Functional badge decision cites the workflow execution log from dimension D3 and the claim mapping file from dimension D4. All references include hyperlinks to the corresponding run directories (Sec IV.D, p6)
- **UC4** = 2 (medium): For every score from dimension D1 to D5, the report enumerates the specific checklist items from ARTIFACTGUIDE that were satisfied or violated. Each item is directly linked to the corresponding command output (Sec IV.D, p6)
- **UC5** = 0 (high): no evidence found
- **UC6** = 0 (high): no evidence found
- **UC7** = 0 (high): no evidence found
- **UC8** = 0 (high): no evidence found
- **B1** = 5 (high): it should be designed as a reviewer-oriented evidence assistant that makes route choices explicit, records intervention burden ... and localizes where human adjudication is still needed (Sec VI.A, p10)
- **B2** = 5 (high): E2: reviewer runs representative workflow and obtains valid output; E3: reviewer runs full or near-full main workflow and obtains paper-facing result objects (Table II, p4)
- **B3** = 4 (high): the input to ARTIFACTCOPILOT is the paper and its corresponding artifacts (Sec IV.A, p5)
- **B4** = 5 (high): Badge rules: Available: GC1-GC3 pass; Functional: Available, D2-D4 >= WA, E2 or above, no I4; Reusable: Functional, both D1 and D5 >= WA, transfer validation succeeds, no I4 (Table II, p4)
- **B5** = 2 (medium): The final phase produces a structured review report in both Markdown and JSON formats ... For each Available, Functional, and Reusable badge, the report explicitly states the deterministic rule applied (Sec IV.D, p6)

### Artisan

- **UC1** = 0 (high): no evidence found
- **UC2** = 0 (high): no evidence found
- **UC3** = 1 (high): Sec 2.1, p2: "given a research paper p, a table t from p, and an artifact URL u, automatically generate a reproduction script s that reproduces t using the artifact."
- **UC4** = 0 (medium): Sec 1, p1: "authors can use Artisan to check whether their prepared artifact is sufficiently well-packaged and well-documented to facilitate reproduction."
- **UC5** = 1 (high): Sec 4.6, p9: "By using Artisan, we encounter 20 inconsistencies between papers and their artifacts... it is ultimately up to a human expert to decide if there is indeed a bug, and if so, where the bug lies."
- **UC6** = 1 (medium): Sec 2.1, p2: "If the provided artifact is non-functional (e.g., an obsolete Dockerfile), s may fix it, so the table can still be reproduced." Footnote 1: "most artifacts do not include such scripts."
- **UC7** = 0 (high): no evidence found
- **UC8** = 0 (high): no evidence found
- **B1** = 3 (medium): Sec 1, p1-2: "First, authors can use Artisan to check whether their prepared artifact is sufficiently well-packaged... Second, evaluators of artifacts can use Artisan to automate an important part of the evaluation process"
- **B2** = 5 (high): Sec 2.5.1, p4: "the output judge runs the submitted script in a fresh container environment and classifies the result into four categories"
- **B3** = 3 (medium): Sec 2.1, p2: "given a research paper p, a table t from p, and an artifact URL u"; Sec 4.2.3, p8: "55% of the steps are spent on exploration (grep, cat, and sed commands)"
- **B4** = 1 (high): Sec 5.1, p10: "Artisan expects the outputs to exactly match the expected results and thus cannot reproduce nondeterministic results."
- **B5** = 3 (medium): Sec 2.2, p2: "Otherwise, Artisan outputs the submitted reproduction script."; Sec 2.5.2, p5: method judge classifies into "Copied results", "Last-mile reproduction", "Full reproduction"

### Amougou Mbida & Angermeir

- **UC1** = 0 (high): no evidence found
- **UC2** = 0 (high): no evidence found
- **UC3** = 0 (medium): Evaluation demands evidence-based reasoning with explicit citations (e.g., file paths, code snippets), defaulting to a fail status if required evidence is missing. (Sec 3, Phase 4, p.5)
- **UC4** = 2 (high): we implement a multi-agent prototype that inspects replication packages and produces evidence-grounded improvement reports. (Abstract, p.1); 'diagnoses specific shortcomings and generates a to-do list for artifact improvement' (Sec 5, p.10)
- **UC5** = 0 (high): Because some requirements demand paper-text analysis rather than artifact inspection, we distinguish between the full set and a subset of 31 operationalized criteria that are fully amenable to automated, artifact-based evaluation. (Sec 4 RQ1, p.6)
- **UC6** = 0 (high): no evidence found
- **UC7** = 0 (high): no evidence found
- **UC8** = 0 (high): no evidence found
- **B1** = 2 (medium): We hope that our contribution in the long-term can support authors in improving their research artifact quality even before artifact review and to consequently reduce review overhead in artifact review tracks. (Sec 1, p.2)
- **B2** = 1 (high): evaluate each subtopic concurrently within isolated reasoning contexts over a shared, read-only filesystem, gathering evidence via pattern search and hybrid retrieval (lexical and semantic). (Sec 3, Phase 4, p.5)
- **B3** = 4 (medium): The input paper is converted from PDF to markdown to extract key metadata (title, abstract, artifact details) and a global summary... A sub-agent extracts artifact references (e.g., GitHub, Zenodo, OSF) from the paper summary. (Sec 3, Phases 1-2, p.4)
- **B4** = 5 (high): We consolidate 380 requirements from 34 sources into 51 reproducibility criteria, of which 31 are operationalized for automated artifact-based evaluation. (Abstract, p.1)
- **B5** = 3 (high): The structured output is transformed into a hierarchical markdown report aggregating evaluation metadata, paper summaries, and detailed criterion assessments linked to their guidelines. (Sec 3, Phase 5, p.5)

### OpenART (abstract)

- **UC1** = 1 (medium): "generate structured artifact documentation, including required components, dependencies, and execution steps" (abstract)
- **UC2** = 0 (low): no evidence found
- **UC3** = 0 (low): "extracts explicit and implicit execution-relevant information from papers to generate structured artifact documentation" (abstract)
- **UC4** = 2 (medium): "automated identification of missing or inconsistent components in existing packages" (abstract)
- **UC5** = 1 (low): "automated identification of missing or inconsistent components in existing packages" (abstract)
- **UC6** = 1 (medium): "derives an expected artifact layout that enables (i) guided preparation of replication packages from scratch" (abstract)
- **UC7** = 0 (low): no evidence found
- **UC8** = 0 (low): no evidence found
- **B1** = 1 (medium): "mixed-method user study with SE researchers across two realistic workflows: artifact creation and artifact completion" (abstract)
- **B2** = 1 (low): "Leveraging both foundation and fine-tuned large language models, OpenART extracts explicit and implicit execution-relevant information from papers" (abstract)
- **B3** = 1 (high): "automatically deriving actionable artifact specifications directly from companion manuscripts" (abstract; title says 'Manuscript-Driven')
- **B4** = 1 (low): "Despite substantial progress in artifact availability and badging" (abstract, background only)
- **B5** = 4 (medium): "generate structured artifact documentation ... From this documentation, OpenART further derives an expected artifact layout" (abstract)

### Snelleman et al.

- **UC1** = 0 (high): no evidence found
- **UC2** = 0 (high): no evidence found
- **UC3** = 1 (medium): "correct any mistakes made by the LLM in its phrasings, links between hypotheses, experiments and interpretations, as well as experiment details, such as measured outcomes" (Method, p.3); Table 2 lists 'Experiment Hypothesis links', 'Interpretation Experiment links' (p.4)
- **UC4** = 0 (high): no evidence found
- **UC5** = 0 (high): "in some cases, a clear phrasing of hypotheses or research questions within a given study is essential for capturing the essence of the work" (Discussion, p.5)
- **UC6** = 0 (medium): "automatically extract the problem statement of reproducibility for any empirical AI study" (Introduction, p.1)
- **UC7** = 0 (high): no evidence found
- **UC8** = 0 (high): no evidence found
- **B1** = 4 (medium): "In order to relieve independent investigators of the workload, the automatisation of reproducing studies to any extent would have a substantial impact." (Introduction, p.1)
- **B2** = 1 (high): "We constructed a relatively simple prompt and presented this, together with the PDF of each publication, to Google Gemini 2.5 Pro" (Method, p.3)
- **B3** = 1 (high): "We applied our representation to automatically extract the problem from any PDF and reviewed its capabilities on 20 studies" (Conclusion, p.5)
- **B4** = 1 (medium): "We consider the following notion of reproducibility, based on Gundersen (2021) ... we derive our problem statement, formulated in terms of the scientific method" (Background, p.3)
- **B5** = 4 (medium): "prompted the model to produce the hypotheses, experiments and interpretations of outcomes for the 20 publications" (Method, p.3); "Authors were given the opportunity to adapt the phrasing." (Fig. 5 caption, p.4)

### ReproRepo

- **UC1** = 0 (high): no evidence found
- **UC2** = 0 (high): no evidence found
- **UC3** = 1 (medium): "evidence": [{"path": "...", "line_refs": "...", ...}], ... "affected_claims_or_metrics": ["..."] (Sec. E.1 agent prompt, findings.json schema, p.17)
- **UC4** = 1 (high): missing or ambiguous data/checkpoint/model artifacts - broken or incomplete installation/dependency instructions - missing scripts/configs/paths referenced by the paper or README (Sec. E.1 agent prompt, p.16)
- **UC5** = 1 (high): Silent wrong setup: the released setup does not match the paper's intended configuration, artifact scope, checkpoint, data provenance, or evaluation path (Sec. B.2, p.12); finding category paper_repo_mismatch (Sec. E.1, p.17)
- **UC6** = 0 (high): no evidence found
- **UC7** = 0 (high): no evidence found
- **UC8** = 0 (high): no evidence found
- **B1** = 4 (medium): The property is practically useful for reviewer or author triage when humans only have limited bandwidth to check a few potential issues. (Sec. 5.2, p.8)
- **B2** = 1 (high): The agent is instructed to perform a static inspection without environment setup and code execution. GPU compute is withheld from the workspace. (Sec. 3.2, p.4)
- **B3** = 4 (medium): Adding the paper improves performance across all metrics ... repository inspection alone can often identify the broad failure region, as reflected by the strong code-only semantic match rates (Sec. 5.2, p.8)
- **B4** = 1 (high): Our key idea is to use real reproduction pain as supervision: instead of relying on experts to manually design tasks, inject errors, or write evaluation rubrics, ReproRepo leverages GitHub issues (Sec. 1, p.1)
- **B5** = 3 (medium): "issue_title": "...", "user_symptom": "...", "trigger_context": "...", "root_cause": "...", ... "impact": "..." (Sec. E.1 findings.json schema, p.17); main metrics are EM rate and SM rate (Sec. 3.3, p.4)

### Heye et al.

- **UC1** = 0 (high): no evidence found
- **UC2** = 0 (high): no evidence found
- **UC3** = 0 (medium): "(ii) detect potential inconsistencies between claims and submitted artifacts" (Sec. I, p.1)
- **UC4** = 1 (high): "The final deliverable of this stage is either a runnable container image ... or a structured error report that pinpoints issues the agent faced." (Sec. III-C, p.4)
- **UC5** = 1 (medium): "The classifier outputs which pitfalls are most likely present. The report highlights potential design or evaluation flaws, providing reviewers with insights" (Sec. III-D, p.4)
- **UC6** = 1 (medium): "The final deliverable of this stage is either a runnable container image ready for further analysis by an AE expert" (Sec. III-C, p.4)
- **UC7** = 0 (high): "we derive a unique concept vector for each pitfall individually using a set of training papers" (Sec. III-D, p.4)
- **UC8** = 0 (high): no evidence found
- **B1** = 5 (high): "we aim to provide automated support for reviewers of scientific contributions to improve the scalability of AE" (Sec. I, p.1)
- **B2** = 4 (high): "The LLM-based agent used in this stage iteratively issues shell commands to clone the repository, install dependencies, and compile and execute code" (Sec. III-A, p.3)
- **B3** = 3 (high): "the full paper text and, in the case of RATE, a Readme file associated with the submission's code artifact" (Sec. IV-B, p.4)
- **B4** = 3 (medium): "We focus on Arp et al.'s [35] taxonomy of ten common pitfalls in AI-driven cybersecurity research" (Sec. III-D, p.4)
- **B5** = 2 (high): "The resulting score s reflects how strongly the paper's text aligns with the distilled reproducibility concept vector" (Sec. III-B, p.4)

### Paper2Code family

- **UC1** = 0 (high): no evidence found
- **UC2** = 0 (high): no evidence found
- **UC3** = 1 (medium): ReproAgent Sec 3.4, p.5: 'Each generated file returns a provenance record linking file regions back to the requirement and evidence IDs they realize'; units carry 'src records the paper source span' (Sec 3.1, p.3).
- **UC4** = 0 (high): no evidence found
- **UC5** = 1 (medium): PaperCoder Fig. 35, p.56 (Overall Plan artifact): '7. AMBIGUITIES & UNCERTAIN DETAILS. Tokenization & BPE/Word-piece: The exact BPE merge operations and vocabulary processing details are not fully described.'
- **UC6** = 2 (high): PaperCoder Abstract, p.1: 'we introduce PaperCoder, a multi-agent LLM framework that transforms machine learning papers into operational code repositories.'
- **UC7** = 0 (medium): no evidence found
- **UC8** = 1 (medium): AutoReproduce Sec 3.2.2, p.3: 'the research agent identifies the top-k relevant papers (default k=3) from the source paper's references ... retrieves the manuscripts via the ArXiv API, summarizes their content, and identifies linked code repositories.'
- **B1** = 3 (medium): PaperCoder Abstract, p.1: 'corresponding code implementations are often unavailable, making it slow and labor-intensive for researchers to reproduce results and build upon prior work.'
- **B2** = 4 (medium): ReproAgent Sec 3.4, p.5: 'Runtime validation runs dependency installation, import checks, declared entry points, smoke commands, and benchmark commands when available.'
- **B3** = 1 (high): PaperCoder Sec 3.2, p.3: 'generating code repositories directly from machine learning papers (without access to pre-existing artifacts or implementations, such as skeleton code).'
- **B4** = 1 (high): no evidence found
- **B5** = 3 (medium): PaperCoder Sec 3.1, p.3: 'we define this task as a function (or a model) M that maps a paper R to a corresponding code repository C.'

### Repo2Run / ExecutionAgent

- **UC1** = 0 (high): "the desired output consists of two scripts: one to create an isolated environment, such as a container, and one to build the project and run its tests" (ExecutionAgent Sec. 2.1, p.3)
- **UC2** = 0 (high): no evidence found
- **UC3** = 0 (high): no evidence found
- **UC4** = 0 (medium): "Otherwise, the control center provides feedback to the LLM agent, pointing out what exactly is missing, and asks for a new command." (ExecutionAgent Sec. 2.4.4, p.10)
- **UC5** = 0 (high): no evidence found
- **UC6** = 1 (high): "given a code repository, Repo2Run iteratively builds the Docker image, runs unit tests based on the feedback of the building, and synthesizes the Dockerfile until the entire pipeline is executed successfully" (Repo2Run Abstract, p.1)
- **UC7** = 0 (high): no evidence found
- **UC8** = 0 (high): no evidence found
- **B1** = 3 (medium): "a valuable tool for developers, automated programming tools, and researchers that need to execute tests across a wide variety of projects" (ExecutionAgent Abstract, p.1); "Repo2Run will serve as the foundational infrastructure ... facilitating the community to efficiently scale up their executable code data" (Repo2Run Sec. 1, p.3)
- **B2** = 5 (high): "We provide the agent with the capability to execute any command available in a Linux terminal via the linux_terminal tool." (ExecutionAgent Sec. 2.4.2, p.9); Repo2Run: "The internal environment is a Docker container where the agent can execute actions restricted to a container, e.g., test running." (Sec. 3, p.3)
- **B3** = 5 (high): "Input: Repository URL u  Output: Scripts to set up an environment and run tests" (ExecutionAgent Algorithm 1, p.5); "Given a code repository, Repo2Run synthesizes a runnable Dockerfile" (Repo2Run Sec. 3, p.3)
- **B4** = 1 (high): "Instead of engineering such prompts manually, we leverage the impressive knowledge of LLMs to generate prompts with up-to-date guidelines targeted at the given project." (ExecutionAgent Sec. 2.3.1, p.6)
- **B5** = 2 (medium): "checking whether three files exist: a file to create a container (e.g., a Dockerfile), an installation script to be executed within the container, and a file that contains the test results (i.e., the number of passed, failed, and skipped tests)" (ExecutionAgent Sec. 2.4.4, p.10)

### CORE / PaperBench

- **UC1** = 0 (medium): please also include a README.md file that describes what you were able to achieve in your reproduction attempt, explains how your codebase relates to various parts of the reproduction (PaperBench App. F.3, Fig. 13, p.29)
- **UC2** = 0 (high): no evidence found
- **UC3** = 1 (medium): Explore the files provided for the submission along with the output logs to identify the parts that are relevant to the resolution criteria ... be explicit about which files you are referring to (PaperBench Fig. 9, p.27)
- **UC4** = 0 (medium): If either 'reproduce.sh' or 'reproduce.log' is missing, you should consider any criteria relying on it to have failed. (PaperBench Fig. 8, p.26)
- **UC5** = 1 (low): We manually create an addendum for each paper containing clarifications from the paper's original authors. The addendums also clarify when parts of the paper are out of scope. (PaperBench Sec. 3.2, p.6)
- **UC6** = 2 (medium): The candidate must produce a submission which consists of a repository including all the code required to reproduce the paper's empirical results. This repository must include a reproduce.sh file at its root (PaperBench Sec. 2.1, p.2)
- **UC7** = 0 (high): no evidence found
- **UC8** = 0 (high): no evidence found
- **B1** = 0 (medium): authors could verify their work's reproducibility before publication, independent researchers could more easily replicate past studies, and conference organizers and journal editors could efficiently assess the reproducibility of submissions (CORE-Bench Sec. 1, p.4)
- **B2** = 5 (high): we copy its submission to a fresh VM running an Ubuntu 24.04 image with access to an A10 GPU. We execute the submission's reproduction script to generate results from a clean start. (PaperBench Sec. 2.2, p.3)
- **B3** = 3 (medium): CORE-Bench (Siegel et al., 2024) tasks agents to reproduce the results of a research paper given its repository. In contrast, PaperBench tasks agents to replicate the results of a research paper from scratch. (PaperBench Sec. 6, p.9)
- **B4** = 3 (medium): Each paper is accompanied by a manually created rubric, which specifies all the necessary outcomes for replicating the paper in detail; resulting in a total of 8,316 individually gradable outcomes (PaperBench Sec. 1, p.1)
- **B5** = 1 (high): We report task accuracy as the main metric, which is the proportion of tasks for which all of the task questions have been answered correctly. (CORE-Bench Sec. 3, p.9)

### LEDGER / Argonne

- **UC1** = 0 (high): no evidence found
- **UC2** = 0 (high): no evidence found
- **UC3** = 1 (medium): Sec 1, p.2 (LEDGER): 'Typed semantic edges such as uses, produces, checked_by, and supports describe how actions, artifacts, validation steps, and claims relate to one another.'
- **UC4** = 0 (medium): no evidence found
- **UC5** = 1 (medium): Sec 4.1, p.4 (Argonne): 'A reviewer or operator can ask for all claims in the final draft whose evidence does not exist, whose extracted value differs from the cited metric, or whose verifier failed.'
- **UC6** = 0 (high): no evidence found
- **UC7** = 0 (high): no evidence found
- **UC8** = 0 (high): no evidence found
- **B1** = 4 (medium): Sec 1, p.1 (LEDGER): 'productive use depends not only on generation speed, but also on the user's ability to audit the recorded work.'
- **B2** = 3 (medium): Sec 3.1, p.3 (LEDGER): 'Our prototype tests this capture path on Codex using lifecycle hooks and transcript reconstruction, but the approach is not specific to Codex.'
- **B3** = 5 (medium): Sec 3, p.3 (LEDGER): 'The graph is built from captured records and a session transcript, then organized into Trace Records, Evidence Nodes, Workflow Nodes, Artifact, and typed Semantic Edges.'
- **B4** = 2 (medium): Sec 5, p.4 (Argonne): 'we propose five minimum requirements for autonomous scientific agents for research venues, benchmarks, and tool builders'
- **B5** = 3 (medium): Sec 1, p.2 (LEDGER): 'The resulting graph makes the audit path explicit: a reviewer can start from a conclusion, follow support edges to relevant artifacts and actions, and inspect the source records behind them.'
