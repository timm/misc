# All general laws (94)

Merged from the five step-6 files (`laws_dynamic-testing.json`, `laws_analysis-and-verification.json`, `laws_none-and-human.json`, `laws_learning-based-A.json`, `laws_learning-based-B.json`). Order: the 57 interesting laws in the 10 themes, then 9 interesting laws outside themes, then 28 not interesting. The ladder, method and caveats are in `LAWS.md`; the machine-readable version is `laws_merged.json`.

Field key: **When** = condition C. **Because** = mechanism M (source: paper = a paper gave the because: chain; reader = inferred). **Unless** = rebuttal R. **Stated** = yes (a paper states the general form), implied (instances support it), reader (synthesis). Instance ids are `<area>/<paper-slug>:<line>` in `spec_study/icse26_<area>/`.

## Contents

- T1 Shrink what the expensive step sees: LB-B-1, LB-B-3, LB-B-7, LB-B-10, LB-A-1, LB-A-10, AV-1, AV-10, AV-11
- T2 Missing context, not model size, drives LLM failure: LB-B-8, LB-B-13, LB-B-15, LB-B-16, NH-11, LB-A-9
- T3 Evaluation is only as strict as its oracle: DT-7, LB-B-17, LB-A-12, LB-A-7, DT-8, LB-A-14
- T4 Irrelevant surface cues sway learned judgement: NH-12, LB-B-11, LB-B-12, LB-B-14
- T5 Claims argued, not measured: AV-7, AV-9, AV-12, LB-B-6, LB-A-3, NH-3
- T6 Every gain is a trade-off at a threshold: AV-8, LB-B-18, LB-A-5, LB-A-8, LB-B-4, DT-3
- T7 Guidance fitted to the subject beats generic guidance: DT-1, DT-2, NH-5, LB-B-2, LB-A-4, DT-10
- T8 Difficulty scales with size, validation does not: NH-4, NH-7, LB-A-13, LB-A-2, AV-3, DT-5
- T9 Combining members gives diminishing returns: LB-B-9, LB-A-11, LB-A-6
- T10 Adoption depends on trust, not accuracy alone: NH-1, NH-6, NH-8, LB-B-5, AV-5
- Interesting, no theme: AV-2, AV-4, AV-6, DT-4, DT-6, DT-9, NH-10, NH-2, NH-9
- Not interesting: AV-13, AV-14, AV-15, AV-16, AV-17, DT-11, DT-12, DT-13, LB-A-15, LB-A-16, LB-A-17, LB-B-19, LB-B-20, LB-B-21, LB-B-22, LB-B-23, LB-B-24, LB-B-25, LB-B-26, LB-B-27, NH-13, NH-14, NH-15, NH-16, NH-17, NH-18, NH-19, NH-20


## T1 Shrink what the expensive step sees

### LB-B-1

When a learning-based tool confines the LLM to a narrow, checkable step and hands everything else to deterministic scaffolding (a compilable skeleton, a compiler IR, a shared subgoal cache, graph validation), the pipeline becomes feasible at project or proof scale, because errors from one LLM call can no longer cascade across modules or searches, unless the input falls outside what the scaffolding models (multi-threaded projects, unseen obfuscation families).

- **When:** the LLM is isolated to one bounded sub-task whose output is validated or consumed by deterministic machinery
- **Techniques:** translation, deobfuscation, proof-synthesis
- **Effect:** enables project-level / proof-level pipelines and contains LLM errors (design)
- **Because** (paper): a compilable skeleton removes cross-module dependencies from each LLM call so cascading errors are avoided (evoc2rust:48); repairs are kept only if they lower the error count (evoc2rust:57); LLM detections inconsistent with the fixed dependency graph are discarded (cascade:44); a shared cache lets one model's partial search complete another's (proofcoop:74)
- **Unless:** threading(project) = multi (evoc2rust:48); obfuscation(input) other than obfuscator-io (cascade:40)
- **Stated:** implied · **Interesting:** yes — cross-area (translation, deobfuscation, Coq proof search) mechanism: the benefit comes from shrinking the LLM's scope, not from a better LLM; the unless clauses mark where the scaffolding stops covering inputs
- **Papers (3; ai4se, formal):** cascade_llm_powered_javascript_deobf, evoc2rust_a_skeleton_guided_framewor, proofcoop_collaborative_automated_fo
- **Instances (7):** ai4se/evoc2rust_a_skeleton_guided_framewor:48, ai4se/evoc2rust_a_skeleton_guided_framewor:52, ai4se/evoc2rust_a_skeleton_guided_framewor:57, ai4se/cascade_llm_powered_javascript_deobf:40, ai4se/cascade_llm_powered_javascript_deobf:42, ai4se/cascade_llm_powered_javascript_deobf:44, formal/proofcoop_collaborative_automated_fo:74
- **Stem:** learning-based | proposed | design

```
enable(technique(pipeline, project-level, approach=learning-based, llm-scope=confined))
  because: skeleton(stubs) = compilable → errors(dependencies, cascading) = avoided → correctness(outputs) increasing-in llm-scope-confinement
  because: detections(llm) inconsistent-with dependency-graph = discarded → errors(llm, propagated) = reduced
  because: cache(subgoal-proofs) = shared → power(model, subgoals) = increased
  unless: threading(project) = multi | obfuscation(input) = unseen-family
```

### LB-B-3

When a learning-based tool decomposes a large problem (requirements into sub-requirements, theorems into subproofs, searches into automation-first steps, projects into modules), it scales to larger inputs, because each unit sees fewer variables, a narrower search tree, and a subgoal small enough for automation, unless cross-unit dependencies (third-party functions) survive decomposition.

- **When:** the tool decomposes inputs into units handled independently
- **Techniques:** spec-inference, proof-synthesis, translation
- **Effect:** more theorems/requirements/projects handled per invocation; per-unit size (variables, search width/depth) decreases
- **Because** (paper): sub-requirement proxies use only their own variables so trace variables decrease with decomposition (automating_requirements:111); fail-safe localization to the deepest failing subproof lets a hammer finish the subgoal (cobblestone:188); offloading tried-first automation tactics reduces search-tree width and depth (proofcoop:133)
- **Unless:** dependencies(functions, third-party) = present (evoc2rust:181); traces may get slightly longer even as variables drop (automating_requirements:116)
- **Stated:** implied · **Interesting:** yes — mechanism shared across spec inference, Coq proof synthesis and C-to-Rust translation; the unless clause names the condition (surviving cross-unit dependencies) under which decomposition stops paying
- **Papers (4; ai4se, formal):** automating_requirements_formalizatio, cobblestone_a_divide_and_conquer_app, evoc2rust_a_skeleton_guided_framewor, proofcoop_collaborative_automated_fo
- **Instances (5):** formal/automating_requirements_formalizatio:111, formal/automating_requirements_formalizatio:100, formal/cobblestone_a_divide_and_conquer_app:188, formal/proofcoop_collaborative_automated_fo:133, ai4se/evoc2rust_a_skeleton_guided_framewor:181
- **Counter-instances (1):** formal/automating_requirements_formalizatio:116
- **Stem:** learning-based | proposed | scale

```
improve(scale(problems, approach=learning-based, decomposition=on))
  because: proxy(sub-requirement) = own-variables-only → variables(trace) decreasing-in decomposition
  because: localization(error) = deepest-subproof → hammer(subgoal) = applied → count(theorem, proven) increasing-in localization
  because: tactics(automation, tried-first) = offloaded → size(search-tree) = reduced
  unless: dependencies(functions, third-party) = present
```

### LB-B-7

When every LLM proposal passes through a sound deterministic component (the Coq kernel checking each candidate proof; a semantics-preserving IR doing the actual transformations), the final artefact is correct despite unsound LLMs, because nondeterminism is confined to proposals that are checked or to a small step with its own definable metric, unless the checked property is weaker than the claimed one (no functional-equivalence check, false positives left unmeasured).

- **When:** LLM output is either checked by a sound checker or limited to proposing inputs for semantics-preserving transformations
- **Techniques:** proof-synthesis, deobfuscation
- **Effect:** correctness/soundness preserved; hallucination errors reduced; verifiability improved
- **Because** (paper): the ITP checks every candidate, so soundness is guaranteed (cobblestone:55); JSIR transformations are semantics-preserving and arithmetic is delegated to the compiler, so LLM hallucination errors are avoided (cascade:129, cascade:132); nondeterminism confined to prelude detection makes a correctness metric definable (cascade:138)
- **Unless:** the guarantee covers only what the checker checks: CASCADE reports no functional-equivalence check (cascade:129 gloss); Citywalk leaves execution-fix false positives unmeasured (citywalk:82)
- **Stated:** implied · **Interesting:** yes — denies the default belief that LLM-based tools cannot give correctness guarantees; mechanism shared across formal proof and JavaScript deobfuscation
- **Papers (2; ai4se, formal):** cascade_llm_powered_javascript_deobf, cobblestone_a_divide_and_conquer_app
- **Instances (4):** formal/cobblestone_a_divide_and_conquer_app:55, ai4se/cascade_llm_powered_javascript_deobf:129, ai4se/cascade_llm_powered_javascript_deobf:132, ai4se/cascade_llm_powered_javascript_deobf:138
- **Stem:** learning-based | proposed | correctness

```
preserve(correctness(outputs, approach=learning-based, checker=sound))
  because: check(proof, coq-itp) = every-candidate → soundness(proof) = guaranteed
  because: transformations(ir) = semantics-preserving ∧ calculations = delegated-to-compiler → errors(llm, hallucination) = reduced
  because: share(nondeterministic, pipeline) = one-step → metric(correctness, step) = definable → verifiability = improved
  unless: property(checked) weaker-than property(claimed)
```

### LB-B-10

When a baseline validates or searches without decomposition (pairwise trace validation over all candidates, exhaustive trace generation, wide depth-first proof search), its cost grows with the candidate or search space (linearly, exponentially, or past a week), so its gap to a decomposing tool widens as problems grow, because nothing prunes the space before the expensive step.

- **When:** the baseline enumerates candidates or search branches without decomposition or pruning
- **Techniques:** spec-inference, proof-synthesis
- **Effect:** validation effort, latency and search time grow with n / width; ratio to the decomposing tool grows with candidates
- **Because** (reader): effort is proportional to candidates (up to n−1 traces), exhaustive generation is 2^n, and DFS width multiplies branches; no step shrinks the space first
- **Unless:** small candidate sets (mean ratio only 1.74× over all requirements vs 2.52× when over 10 candidates)
- **Stated:** implied · **Interesting:** yes — condition (problem size) that changes the magnitude of the cost gap; pairs naturally with LB-B-3's decomposition mechanism
- **Papers (2; formal):** automating_requirements_formalizatio, proofcoop_collaborative_automated_fo
- **Instances (9):** formal/automating_requirements_formalizatio:77, formal/automating_requirements_formalizatio:92, formal/automating_requirements_formalizatio:93, formal/automating_requirements_formalizatio:94, formal/automating_requirements_formalizatio:95, formal/automating_requirements_formalizatio:96, formal/automating_requirements_formalizatio:97, formal/automating_requirements_formalizatio:140, formal/proofcoop_collaborative_automated_fo:143
- **Stem:** learning-based | baseline | cost

```
worsen(cost(validation|search, approach=learning-based, decomposition=off), size(space))
  because: effort(validation, traces) ∝ n ∧ latency(exhaustive) ∝ 2-to-n ∧ time(dfs) increasing-in width → cost increasing-in size(space)
  unless: candidates ≤ small
```

### LB-A-1

When a learning-based tool narrows what the LLM must do (a deterministic pre-filter, decomposition into subgoals, a candidate-halving query, a vote among searchers), its token, query, step or validation cost falls below its baselines because fewer or smaller LLM calls are made, unless the added machinery instead enriches what each call carries or explores (resent few-shot examples, retrieval, loop unrolling, dependency context, the union of every model's tactics), in which case cost rises, often multiplicatively.

- **When:** the proposed machinery removes or shrinks LLM work (pre-filtering inputs, disabling reasoning, splitting goals, choosing maximally distinguishing queries, selecting one searcher's tactic)
- **Techniques:** deobfuscation, proof-synthesis, spec-inference, llm-as-judge, test-generation, llm-fine-tuning
- **Effect:** lowers cost (LLM queries, tokens, search steps, human validation queries); raises it when the machinery enriches per-call context or widens search
- **Because** (paper): A YARA pre-filter leaves under half the samples for the LLM, so queries fall; limiting the LLM to prelude detection lets thinking be switched off, so tokens fall; a balanced distinguishing trace eliminates about half the candidates, so validation effort grows as log n; conversely hard-coded few-shot examples are resent with every query, so tokens rise.
- **Unless:** the machinery enriches every call or widens the search (HoarePrompt few-shot examples and k-induction unrolling, E-Test retrieval, CITYWALK dependency-aware prompts, ProofCoop union tactic prediction); fine-tuning that removes the few-shot examples would remove that token overhead
- **Stated:** implied · **Interesting:** yes — Resolves the stem's better/worse split (7 papers each way) with one condition: whether the added machinery subtracts LLM work or adds LLM input. Holds across ai4se, formal and testing, and within single papers (HoarePrompt without unrolling uses 90% fewer tokens; ProofCoop voting takes 20% fewer steps while union takes 2.3x more).
- **Papers (7; ai4se, formal, testing):** automating_requirements_formalizatio, cascade_llm_powered_javascript_deobf, citywalk_enhancing_llm_based_c_unit, cobblestone_a_divide_and_conquer_app, e_test_e_er_improving_test_suites, hoareprompt_structural_reasoning_abo, proofcoop_collaborative_automated_fo
- **Instances (16):** ai4se/cascade_llm_powered_javascript_deobf:120, ai4se/cascade_llm_powered_javascript_deobf:123, ai4se/cascade_llm_powered_javascript_deobf:125, formal/cobblestone_a_divide_and_conquer_app:136, formal/cobblestone_a_divide_and_conquer_app:138, formal/cobblestone_a_divide_and_conquer_app:140, formal/cobblestone_a_divide_and_conquer_app:142, formal/automating_requirements_formalizatio:78, formal/automating_requirements_formalizatio:82, formal/automating_requirements_formalizatio:84, formal/automating_requirements_formalizatio:90, formal/automating_requirements_formalizatio:98, formal/proofcoop_collaborative_automated_fo:120, formal/proofcoop_collaborative_automated_fo:122, formal/proofcoop_collaborative_automated_fo:127, formal/hoareprompt_structural_reasoning_abo:150
- **Counter-instances (14):** formal/hoareprompt_structural_reasoning_abo:134, formal/hoareprompt_structural_reasoning_abo:140, formal/hoareprompt_structural_reasoning_abo:143, formal/hoareprompt_structural_reasoning_abo:144, formal/hoareprompt_structural_reasoning_abo:145, formal/hoareprompt_structural_reasoning_abo:146, formal/hoareprompt_structural_reasoning_abo:147, formal/hoareprompt_structural_reasoning_abo:148, formal/hoareprompt_structural_reasoning_abo:149, formal/hoareprompt_structural_reasoning_abo:151, testing/e_test_e_er_improving_test_suites:133, testing/citywalk_enhancing_llm_based_c_unit:217, testing/citywalk_enhancing_llm_based_c_unit:218, formal/proofcoop_collaborative_automated_fo:128
- **Stem:** learning-based | proposed | cost

```
reduce(cost(llm-calls, approach=learning-based, llm-work=narrowed))
  because: share(inputs, prefiltered) increasing → queries(llm) decreasing → cost(llm, tokens) decreasing
  because: eliminations(query, balanced) ≈ half → effort(validation, traces) ∝ log-n
  unless: context(llm, per-call) increasing-in enrichment → tokens(llm) increasing | few-shot, retrieval, unrolling, union-search
```

### LB-A-10

When a proof or program-reasoning problem is decomposed into smaller pieces (recursive subgoals, isolated program blocks, subgoals shared across searchers), LLM-based tools succeed more often, because small goals fall within reach of cheap complete tools such as hammers and of local compositional reasoning, and shared subgoals shrink the search tree, unless the decomposition enlarges the search (adding local subtrees to a union search), where proof-assistant errors grow and fewer theorems are proved.

- **When:** the LLM-based prover or checker splits the goal and solves or checks the pieces separately
- **Techniques:** proof-synthesis, llm-as-judge
- **Effect:** raises theorems proved, added value over prior tools, and bug-detection success
- **Because** (paper): Decomposed goals are smaller and hammer success falls with goal size; replacing admits with hammer calls lets the hammer try more goals as recursion deepens; reasoning about blocks in isolation captures loop semantics locally; shared subgoals reduce the union search tree, while added local subtrees enlarge it and raise Coq errors.
- **Unless:** decomposition enlarges the search tree (ProofCoop union+local -6%); without a hammer to close subgoals the added value can drop to zero (Cobblestone without CoqHammer on Wigderson100); subgoal sharing gives little unless enough search rounds
- **Stated:** implied · **Interesting:** yes — Three papers give compatible paper-stated mechanisms (goal size vs hammer reach; search-tree size), and ProofCoop's own ablation gives the condition that flips the sign. Single area (formal), so its generality beyond proofs is untested.
- **Papers (3; formal):** cobblestone_a_divide_and_conquer_app, hoareprompt_structural_reasoning_abo, proofcoop_collaborative_automated_fo
- **Instances (12):** formal/cobblestone_a_divide_and_conquer_app:156, formal/cobblestone_a_divide_and_conquer_app:160, formal/cobblestone_a_divide_and_conquer_app:162, formal/cobblestone_a_divide_and_conquer_app:163, formal/cobblestone_a_divide_and_conquer_app:166, formal/cobblestone_a_divide_and_conquer_app:167, formal/cobblestone_a_divide_and_conquer_app:174, formal/proofcoop_collaborative_automated_fo:179, formal/proofcoop_collaborative_automated_fo:98, formal/hoareprompt_structural_reasoning_abo:62, formal/hoareprompt_structural_reasoning_abo:68, formal/hoareprompt_structural_reasoning_abo:107
- **Counter-instances (3):** formal/proofcoop_collaborative_automated_fo:181, formal/cobblestone_a_divide_and_conquer_app:176, formal/proofcoop_collaborative_automated_fo:100
- **Stem:** learning-based | proposed | effectiveness

```
improve(effectiveness(proofs, approach=learning-based, decomposition=on))
  because: size(goal, decomposed) < size(goal, original) → success-rate(hammer, goal) decreasing-in goal-size → success-rate(theorem, hammer) increasing-in decomposition
  because: size(search-tree, union) = reduced-by-shared-subgoals → steps(search) = fewer → theorems increasing
  unless: size(search-tree, union) increasing-in local-subtrees → errors(coq, proof-assistant) increasing-in size → theorems decreasing
```

### AV-1

When verification is re-run or re-scoped, narrowing what is analysed (method-level change impact, minimal loop unwinding, only-necessary files) cuts run time because fewer specs, files and bounds must be processed, unless the scoping work itself costs more than the reanalysis it avoids (already-fast coarse baselines, library-update-heavy revisions, disabling monitors on many non-impacted methods, instrumentation that cannot be undone).

- **When:** verification or runtime monitoring whose scope can be narrowed to what a change or unit touches
- **Techniques:** runtime-verification, bounded-model-checking
- **Effect:** time and memory decrease (speedup > 1) relative to coarser-scoped runs
- **Because** (paper): finer granularity of impact analysis means fewer affected specs and a smaller unit, so less monitoring or checking is done (paper because: granularity(method-level) -> count(specs, affected) lower -> speedup > 1; bounds(unwinding) minimal -> scope limited -> exec-time < 60 s)
- **Unless:** the extra cost of fine-grained analysis or monitor disabling exceeds the savings: baseline already fast (Ps3cl), 35-75% library-update revisions force remonitoring of all classes, instrumentation dominates and cannot be undone
- **Stated:** implied · **Interesting:** yes — the worse instances are explained by a single condition (scoping overhead vs avoided reanalysis) that flips the direction; mechanism and rebuttal both come from paper because/unless lines
- **Papers (2; formal):** do_unit_proofs_work_an_empirical_stu, fine_grained_analyses_for_evolution_
- **Instances (19):** formal/do_unit_proofs_work_an_empirical_stu:147, formal/do_unit_proofs_work_an_empirical_stu:150, formal/do_unit_proofs_work_an_empirical_stu:180, formal/fine_grained_analyses_for_evolution_:45, formal/fine_grained_analyses_for_evolution_:53, formal/fine_grained_analyses_for_evolution_:62, formal/fine_grained_analyses_for_evolution_:65, formal/fine_grained_analyses_for_evolution_:69, formal/fine_grained_analyses_for_evolution_:71, formal/fine_grained_analyses_for_evolution_:72, formal/fine_grained_analyses_for_evolution_:78, formal/fine_grained_analyses_for_evolution_:93, formal/fine_grained_analyses_for_evolution_:95, formal/fine_grained_analyses_for_evolution_:97, formal/fine_grained_analyses_for_evolution_:98, formal/fine_grained_analyses_for_evolution_:99, formal/fine_grained_analyses_for_evolution_:100, formal/fine_grained_analyses_for_evolution_:101, formal/fine_grained_analyses_for_evolution_:170
- **Counter-instances (3):** formal/fine_grained_analyses_for_evolution_:74, formal/fine_grained_analyses_for_evolution_:90, formal/fine_grained_analyses_for_evolution_:102
- **Stem:** analysis-and-verification | proposed | cost

```
improve(cost(verification, approach=analysis-and-verification, scope=narrowed))
  because: granularity(analysis, scope) = finer → count(specs|files|bounds, analysed) decreasing-in granularity → time(verification) decreasing-in granularity
  unless: cost(scoping-analysis) > savings(reanalysis, avoided) | baseline=already-selective, library-update-heavy, instrumentation-dominated
```

### AV-10

When analysis is scoped to what a unit or change actually needs (only-necessary files, no class-level projection, RTS-selected tests), the analysed artefact shrinks (fewer files, models, loop bounds, affected specs, monitors, events) because unaffected parts are excluded, unless dependencies escape that scope (excluded files, library edges missing from the dependency graph), where real defects or new violations are missed.

- **When:** proof or monitoring scope chosen by a dependency or change-impact rule
- **Techniques:** bounded-model-checking, runtime-verification
- **Effect:** count of analysed specs, monitors, events, instrumentation, files, models and bounds decreases
- **Because** (paper): files included only when necessary -> smaller functional unit -> fewer variable models; no class-level projection -> zero specs for unaffected methods (paper because lines)
- **Unless:** scope misses a dependency: only-necessary files missed 3 memory-safety issues; absent library reinstrumentation leaves dependency-graph edges missing -> 1-2 new violations missed
- **Stated:** implied · **Interesting:** yes — the same scoping move in unit proofs and evolution-aware RV has the same failure condition (dependencies outside the scope), a condition that turns a size reduction into missed defects; mechanism and rebuttal from paper lines
- **Papers (2; formal):** do_unit_proofs_work_an_empirical_stu, fine_grained_analyses_for_evolution_
- **Instances (19):** formal/do_unit_proofs_work_an_empirical_stu:101, formal/do_unit_proofs_work_an_empirical_stu:174, formal/do_unit_proofs_work_an_empirical_stu:177, formal/do_unit_proofs_work_an_empirical_stu:179, formal/fine_grained_analyses_for_evolution_:131, formal/fine_grained_analyses_for_evolution_:134, formal/fine_grained_analyses_for_evolution_:137, formal/fine_grained_analyses_for_evolution_:138, formal/fine_grained_analyses_for_evolution_:151, formal/fine_grained_analyses_for_evolution_:152, formal/fine_grained_analyses_for_evolution_:153, formal/fine_grained_analyses_for_evolution_:158, formal/fine_grained_analyses_for_evolution_:159, formal/fine_grained_analyses_for_evolution_:160, formal/fine_grained_analyses_for_evolution_:165, formal/fine_grained_analyses_for_evolution_:166, formal/fine_grained_analyses_for_evolution_:167, formal/fine_grained_analyses_for_evolution_:185, formal/fine_grained_analyses_for_evolution_:200
- **Counter-instances (7):** formal/do_unit_proofs_work_an_empirical_stu:175, formal/fine_grained_analyses_for_evolution_:184, formal/fine_grained_analyses_for_evolution_:186, formal/fine_grained_analyses_for_evolution_:187, formal/fine_grained_analyses_for_evolution_:201, formal/fine_grained_analyses_for_evolution_:202, formal/fine_grained_analyses_for_evolution_:203
- **Stem:** analysis-and-verification | proposed | scale

```
improve(scale(count(analysed), approach=analysis-and-verification, scope=dependency-bounded))
  because: inclusion(files|specs) = only-when-affected → count(files|models|specs|monitors, analysed) decreasing-in scoping
  unless: count(defects|violations, missed) > 0 | dependency(outside-scope)=present, library-edges=missing
```

### AV-11

When analysis is made online or incremental (debugging while inference runs, re-monitoring only impacted classes and specs), a large share of otherwise idle or redundant work is reclaimed because tasks no longer wait for a finished run and unchanged code is not rechecked, unless the baseline is already selective, where the reclaimed share is marginal.

- **When:** online or evolution-aware analysis compared with post-hoc or whole-program re-analysis
- **Techniques:** interactive-debugging, runtime-verification
- **Effect:** share of time spent productively during inference roughly doubles (about 0.23 to 0.55-0.61); share of specs, classes, monitors, events and instrumentation re-processed falls to about 58-72%
- **Because** (paper): analysis possible during inference -> less waiting -> more of task time used while inference runs; fewer affected specs -> less instrumentation (paper because lines)
- **Unless:** baseline already selective (Ps3cl): monitors and events stay at about 99% and instrumentation at about 90%
- **Stated:** reader · **Interesting:** yes — cross-area pattern linking human debugging workflow and RV re-monitoring under one mechanism (avoiding waiting/redundant work), with a condition (baseline selectivity) that shrinks the gain
- **Papers (2; formal, testing):** fine_grained_analyses_for_evolution_, online_and_interactive_bayesian_infe
- **Instances (14):** testing/online_and_interactive_bayesian_infe:95, testing/online_and_interactive_bayesian_infe:98, testing/online_and_interactive_bayesian_infe:100, formal/fine_grained_analyses_for_evolution_:133, formal/fine_grained_analyses_for_evolution_:136, formal/fine_grained_analyses_for_evolution_:142, formal/fine_grained_analyses_for_evolution_:144, formal/fine_grained_analyses_for_evolution_:146, formal/fine_grained_analyses_for_evolution_:148, formal/fine_grained_analyses_for_evolution_:149, formal/fine_grained_analyses_for_evolution_:155, formal/fine_grained_analyses_for_evolution_:156, formal/fine_grained_analyses_for_evolution_:162, formal/fine_grained_analyses_for_evolution_:78
- **Counter-instances (3):** formal/fine_grained_analyses_for_evolution_:150, formal/fine_grained_analyses_for_evolution_:157, formal/fine_grained_analyses_for_evolution_:164
- **Stem:** analysis-and-verification | proposed | prevalence

```
improve(prevalence(share(work, productive), approach=analysis-and-verification, mode=online|incremental))
  because: analysis(timing|scope) = during-run|impacted-only → waiting|reanalysis(redundant) decreasing-in incrementality → share(work, productive) increasing-in incrementality
  unless: gain(share) = marginal | baseline=already-selective
```


## T2 Missing context, not model size, drives LLM failure

### LB-B-8

When a plain LLM baseline is augmented with an external component that supplies what the model lacks (project-dependency and language context; a hammer for low-level proof search), its effectiveness rises for every LLM tried, because failures come from missing context and automation rather than from the model, unless the added step is itself LLM self-revision on a model without instruction tuning, which drifts and hurts (Self-Denoising, −25% pass@1).

- **When:** the added component is external (context retrieval, symbolic automation) rather than the LLM revising its own input
- **Techniques:** commercial-llm, proof-synthesis, llm-prompting
- **Effect:** compile rate, pass rate, line/branch coverage, and proof success rate increase across base LLMs
- **Because** (reader): language- and project-unaware prompts produce uncompilable C++ tests (citywalk:100), so supplying that context helps any base LLM; a hammer closes subgoals the LLM leaves open; for the exception, meta-instructed self-revision needs instruction tuning and semantic drift grows with prompt length (creme:90)
- **Unless:** technique(add-on) = llm-self-revision ∧ tuning(instruction) = none, or long prompts (creme:90)
- **Stated:** implied · **Interesting:** yes — a condition flips the direction: external context/automation helps every model, while LLM self-revision on an untuned model harms; spans testing, formal and ai4se
- **Papers (3; ai4se, formal, testing):** citywalk_enhancing_llm_based_c_unit, cobblestone_a_divide_and_conquer_app, creme_robustness_enhancement_of_code
- **Instances (17):** testing/citywalk_enhancing_llm_based_c_unit:100, testing/citywalk_enhancing_llm_based_c_unit:137, testing/citywalk_enhancing_llm_based_c_unit:138, testing/citywalk_enhancing_llm_based_c_unit:139, testing/citywalk_enhancing_llm_based_c_unit:142, testing/citywalk_enhancing_llm_based_c_unit:143, testing/citywalk_enhancing_llm_based_c_unit:144, testing/citywalk_enhancing_llm_based_c_unit:147, testing/citywalk_enhancing_llm_based_c_unit:148, testing/citywalk_enhancing_llm_based_c_unit:149, testing/citywalk_enhancing_llm_based_c_unit:152, testing/citywalk_enhancing_llm_based_c_unit:153, testing/citywalk_enhancing_llm_based_c_unit:154, formal/cobblestone_a_divide_and_conquer_app:168, formal/cobblestone_a_divide_and_conquer_app:170, formal/cobblestone_a_divide_and_conquer_app:171, formal/cobblestone_a_divide_and_conquer_app:187
- **Counter-instances (1):** ai4se/creme_robustness_enhancement_of_code:90
- **Stem:** learning-based | baseline | effectiveness

```
improve(effectiveness(outputs, approach=learning-based, add-on=external))
  because: prompt(design) = language-unaware → csr(unit-test, cpp) = low → csr increasing-in context(project, language)
  because: hammer(subgoal) = applied → success-rate(theorem, coq) increasing-in automation
  unless: add-on = llm-self-revision ∧ tuning(instruction) = none → drift(semantic) increasing-in prompt-length → pass1(perturbed) decreasing
```

### LB-B-13

When the task needs information that is not in the prompt and is skewed in training data (whether a production scenario is already tested; which candidate implementation is correct), vanilla LLM baselines score near random and their add-ons (RAG, reflective prompting, larger models) help marginally, inconsistently, or even hurt, because models imitate the majority pattern of their training data and larger models imitate it more.

- **When:** the classification or selection target is under-represented in training data and absent from the prompt
- **Techniques:** commercial-llm, llm-confidence
- **Effect:** F1/accuracy ≈ random; decreases with model size; add-on gains depend on model and benchmark
- **Because** (paper): training sets contain more already-tested than not-yet-tested scenarios, so unwanted imitation increases with size and F1 decreases with size (e_test:102); RAG helps only the 6.6% of scenarios whose suite exceeds the context window (e_test:82)
- **Unless:** small models on the already-tested class (e_test:100); a particular model–benchmark pair (Qwen on BigCodeBench, on_llms:145)
- **Stated:** implied · **Interesting:** yes — denies two default beliefs (LLMs beat random; bigger is better) with a paper mechanism; the better instances are separated by model size and model-benchmark pairing
- **Papers (2; ai4se, testing):** e_test_e_er_improving_test_suites, on_llms_internal_representation_of_c
- **Instances (7):** testing/e_test_e_er_improving_test_suites:82, testing/e_test_e_er_improving_test_suites:99, testing/e_test_e_er_improving_test_suites:102, testing/e_test_e_er_improving_test_suites:108, ai4se/on_llms_internal_representation_of_c:144, ai4se/on_llms_internal_representation_of_c:146, ai4se/on_llms_internal_representation_of_c:147
- **Counter-instances (2):** testing/e_test_e_er_improving_test_suites:100, ai4se/on_llms_internal_representation_of_c:145
- **Stem:** learning-based | baseline | quality

```
worsen(quality(classification|selection, approach=learning-based, model=vanilla), size(model))
  because: share(training-set, already-tested) > share(training-set, not-yet-tested) → imitation(llm, unwanted) increasing-in size → f1(classification) decreasing-in size
  because: share(scenarios, suite-over-context) = small → gain(rag) = marginal
  unless: size(model) = small ∧ class = already-tested
```

### LB-B-15

When an LLM task needs context that is not in the prompt or combines several judgement criteria (tests for a closed or complex system; three-criterion summary rating), the share of failed outputs (flaky tests, invalid judgements) rises, because task complexity and missing system context, not model size, drive failure; on simpler single-criterion tasks even small models rarely fail.

- **When:** the task requires unseen system context or multi-criteria judgement
- **Techniques:** commercial-llm, llm-as-judge
- **Effect:** share of flaky generated tests and invalid judgements increases
- **Because** (paper): three criteria make summary judgement more complex than generation judgement, pushing invalid output above 50% (on_the_effectiveness:80); lack of context on the system under test is conjectured as the major cause of flakiness (flakiness:92, gloss, not a because line)
- **Unless:** single-criterion tasks or larger instruction-following models: invalid output ≤ 0.55–3.87% (on_the_effectiveness:46, :83); the smallest model fails least on generation (on_the_effectiveness:50), so size is not the separator
- **Stated:** implied · **Interesting:** yes — condition (task complexity/missing context) separates more-failure from less-failure instances across testing and ai4se, and denies that model size is the separator
- **Papers (2; ai4se, testing):** on_the_effectiveness_of_llm_as_a_jud, on_the_flakiness_of_llm_generated_te
- **Instances (6):** ai4se/on_the_effectiveness_of_llm_as_a_jud:80, ai4se/on_the_effectiveness_of_llm_as_a_jud:50, testing/on_the_flakiness_of_llm_generated_te:50, testing/on_the_flakiness_of_llm_generated_te:56, testing/on_the_flakiness_of_llm_generated_te:92, testing/on_the_flakiness_of_llm_generated_te:165
- **Counter-instances (2):** ai4se/on_the_effectiveness_of_llm_as_a_jud:46, ai4se/on_the_effectiveness_of_llm_as_a_jud:83
- **Stem:** learning-based | subject-model | prevalence

```
increase(prevalence(failed-outputs, approach=learning-based), complexity(task))
  because: criteria(judgment) = 3 → complexity(judgment) > complexity(single-criterion) → share(invalid-output) > 50 %
  because: ~ context(system-under-test) = absent → share(generated, flaky) > share(existing, flaky)
  unless: criteria(judgment) = 1 | context(prompt) = sufficient
```

### LB-B-16

When LLM inputs move outside pretraining knowledge (closed-source, complex C++ systems) or grow past context limits (heavier obfuscation), LLM effectiveness falls (lower compile success, lower response rate), because the model hallucinates APIs it never saw and larger inputs hit token limits.

- **When:** input is unfamiliar to pretraining or large relative to the context window
- **Techniques:** commercial-llm
- **Effect:** compile success rate and response rate decrease
- **Because** (paper): no pretraining knowledge of HANA increases hallucination, so CSR depends on project (flakiness:107); DBMS/C++ complexity lowers CSR (flakiness:112); obfuscated file size grows with level so token-limit errors grow (cascade:70)
- **Unless:** none stated; assumes absence of HANA in pretraining (flakiness:107 assume)
- **Stated:** implied · **Interesting:** yes — cross-area paper mechanisms naming two conditions (familiarity, input size) under which subject LLMs degrade
- **Papers (2; ai4se, testing):** cascade_llm_powered_javascript_deobf, on_the_flakiness_of_llm_generated_te
- **Instances (3):** testing/on_the_flakiness_of_llm_generated_te:107, testing/on_the_flakiness_of_llm_generated_te:112, ai4se/cascade_llm_powered_javascript_deobf:70
- **Stem:** learning-based | subject-model | effectiveness

```
worsen(effectiveness(outputs, approach=learning-based), unfamiliarity(input) | size(input))
  because: knowledge(pretraining, project) = absent → hallucination(generated) increasing-in missing-knowledge → csr(generated) decreasing
  because: size(file) increasing-in level → errors(token-limit) increasing-in level → response-rate decreasing-in level
  assume: knowledge(pretraining, hana) = absent
```

### NH-11

When the target has specific local requirements that surface-level generation does not see (Rust's safety rules in C-to-Rust translation, project-specific goals of test files, precise intent in small edits), generic LLM output is less adequate (low safe-code rates, lower developer acceptance), because output aligned to general patterns misses local requirements, unless the generator is given project-specific context and has the capacity to use it (context-plugin gains grow with model size) or the developer's intent is loose (larger changes).

- **When:** LLM generation for targets with strong local semantics or specific developer intent
- **Techniques:** none (phenomenon: LLM output adequacy), syntactic C-to-Rust translation, multiline code suggestions, C++ unit-test generation with context plugins
- **Effect:** adequacy/acceptance lower where local requirements are specific; higher with context plus model capacity or looser intent
- **Because** (paper): goals(test-code) = specific -> alignment(suggestion, local-practice) = low -> acceptance lower; clarity(intent) decreasing-in change-size -> need(assistance) increasing-in change-size -> acceptance increasing-in change-size
- **Unless:** project context supplied to a sufficiently large model (compile, pass, line and branch coverage gains increase with parameters)
- **Stated:** reader · **Interesting:** yes — Combines worse (safety, test-file acceptance) and better (context gains with scale, larger changes) instances under one condition; paper mechanism; spans ai4se and testing.
- **Papers (3; ai4se, testing):** citywalk_enhancing_llm_based_c_unit, evoc2rust_a_skeleton_guided_framewor, mapping_the_trust_terrain_llms_in_so
- **Instances (7):** ai4se/evoc2rust_a_skeleton_guided_framewor:119, ai4se/mapping_the_trust_terrain_llms_in_so:142, ai4se/mapping_the_trust_terrain_llms_in_so:144, testing/citywalk_enhancing_llm_based_c_unit:136, testing/citywalk_enhancing_llm_based_c_unit:141, testing/citywalk_enhancing_llm_based_c_unit:146, testing/citywalk_enhancing_llm_based_c_unit:151
- **Stem:** none | none | effectiveness

```
decrease(adequacy(llm-output, approach=none), specificity(local-requirements))
  because: goals(target) = specific → alignment(output, local-practice) = low → adequacy(output) = low
  because: clarity(intent) decreasing-in change-size → need(assistance) increasing-in change-size → acceptance(output) increasing-in change-size
  unless: context(project) = supplied ∧ parameters(model) = large
```

### LB-A-9

When LLM-generated code for a compiled target (C++ unit tests, C-to-Rust translation) fails to build, deterministic compiler-guided rule repair recovers far more than LLM self-refinement, and supplying the missing dependency context up front prevents more failures than post-hoc fixing, because most build failures are rule-diagnosable or come from hallucinated, absent frameworks, unless the remaining errors are semantic, where LLM refinement adds more to test pass rate than rules do.

- **When:** LLM code generation or translation into a compiled language with a repair pipeline
- **Techniques:** test-generation, translation
- **Effect:** compile success, execution pass rate and coverage rise most from dependency context and rule-based compiler-feedback repair; LLM refinement adds a smaller, mostly semantic increment
- **Because** (paper): Compiler feedback is rule-diagnosable, so rule fixes clear a broad class of compilation errors; an absent gtest framework makes the LLM hallucinate it, so every test fails to compile without dependency context; feature mappings prevent semantic errors so compiler-guided repair stays sound.
- **Unless:** residual errors are semantic: LLM refinement contributes +8.65 pp CSR after rule repair in CITYWALK and removing it costs EvoC2Rust more test rate (-8.98 pp) than removing rule repair (-4.45 pp)
- **Stated:** yes · **Interesting:** yes — Two papers in different areas agree on an order (context before generation, rules before LLM) with paper-stated mechanisms, and the condition (syntactic vs semantic residue) explains why the LLM step still matters for test pass rate. Denies the common default of letting the LLM fix its own build errors.
- **Papers (2; ai4se, testing):** citywalk_enhancing_llm_based_c_unit, evoc2rust_a_skeleton_guided_framewor
- **Instances (15):** testing/citywalk_enhancing_llm_based_c_unit:165, testing/citywalk_enhancing_llm_based_c_unit:167, testing/citywalk_enhancing_llm_based_c_unit:172, testing/citywalk_enhancing_llm_based_c_unit:174, testing/citywalk_enhancing_llm_based_c_unit:180, testing/citywalk_enhancing_llm_based_c_unit:184, testing/citywalk_enhancing_llm_based_c_unit:186, testing/citywalk_enhancing_llm_based_c_unit:189, testing/citywalk_enhancing_llm_based_c_unit:192, ai4se/evoc2rust_a_skeleton_guided_framewor:59, ai4se/evoc2rust_a_skeleton_guided_framewor:143, ai4se/evoc2rust_a_skeleton_guided_framewor:163, ai4se/evoc2rust_a_skeleton_guided_framewor:164, ai4se/evoc2rust_a_skeleton_guided_framewor:168, ai4se/evoc2rust_a_skeleton_guided_framewor:173
- **Counter-instances (4):** testing/citywalk_enhancing_llm_based_c_unit:188, testing/citywalk_enhancing_llm_based_c_unit:193, ai4se/evoc2rust_a_skeleton_guided_framewor:174, ai4se/evoc2rust_a_skeleton_guided_framewor:169
- **Stem:** learning-based | proposed | effectiveness

```
improve(effectiveness(compile, approach=learning-based, repair=rules-then-llm, context=dependencies))
  because: feedback(compiler) = rule-diagnosable → errors(compilation, rule-fixable) = broad → csr(unit-test) increasing
  because: framework(gtest, prompt) = absent → hallucination(framework) increasing → errors(compilation) = all
  unless: errors(residual) = semantic → test-rate increasing-in llm-refinement
```


## T3 Evaluation is only as strict as its oracle

### DT-7

When dynamic testing is used as a validation or feedback signal, its benefit depends on how faithful its oracle or environment model is, because the dynamic signal can only steer as well as the oracle judges or the model reproduces the environment. Ground-truth sanitizer reproduction tests beat LLM validation after a few patches, and stronger LLMs make test-driven refinement pay off. This does not hold when the oracle is written by a weak model (it misleads repair and scores below no feedback) or when the peripheral model is incomplete (no DMA data is ever injected).

- **When:** the oracle is ground truth or produced by a strong model; the environment model covers the peripherals the subject uses
- **Techniques:** test-based-validation, greybox-fuzzing
- **Effect:** higher acceptance of correct patches, more correct solutions, more coverage; the direction reverses when the oracle or model is unfaithful
- **Because** (paper): oracles(tests, llm) = incorrect → corrections(code, llm) = misguided → correct solutions decreased; models(peripheral, published) = incomplete → triggers(interrupt) missing → injection(dma, data) = 0 → coverage < MMIO fuzzer
- **Unless:** the oracle is generated by a weaker LLM (llama70b: Testing 29% vs Baseline 30%); peripheral models lack expert rules (SEmu below MultiFuzz on the DICE set)
- **Stated:** implied · **Interesting:** yes — Spans three areas (APR validation, LLM code generation, firmware fuzzing). Oracle or model faithfulness flips the direction: hoareprompt:169 is normalized as 'better' but the source DSL shows Testing 29% vs Baseline 30% for llama70b, a regression. Both counter-instances give a mechanism.
- **Papers (3; ai4se, formal, testing):** abstain_and_validate_a_dual_llm_poli, dyma_fuzz_dynamic_direct_memory_acce, hoareprompt_structural_reasoning_abo
- **Instances (2):** ai4se/abstain_and_validate_a_dual_llm_poli:161, formal/hoareprompt_structural_reasoning_abo:172
- **Counter-instances (2):** formal/hoareprompt_structural_reasoning_abo:169, testing/dyma_fuzz_dynamic_direct_memory_acce:147
- **Stem:** dynamic-testing | baseline | effectiveness

```
improve(effectiveness(validation, approach=dynamic-testing, oracle=faithful))
  because: test(reproduction, sanitizer) = available → plausibility(patches, repro-filtered) = high → accept(correct-patch) increasing
  because: oracles(tests, llm) = incorrect → corrections(code, llm) = misguided → correct(codegen) = decreased
  because: models(peripheral) = incomplete → injection(dma, data) = 0 → coverage(fuzzing) < mmio-fuzzer
  unless: capability(oracle-generator) = low | expertise(peripheral-rules) = unavailable
```

### LB-B-17

When LLM agents or generators are re-evaluated under stricter validation or harder replication settings (running all developer tests, filtering suspicious patches, a complex C++ DBMS instead of the original study's code base), their headline effectiveness shrinks (up to about 19 points on SWE-bench Verified) and the gaps between tools narrow, because lenient validation accepts plausible-but-wrong outputs.

- **When:** evaluation is made stricter or moved to a harder setting than the original benchmark
- **Techniques:** llm-agent, commercial-llm
- **Effect:** resolve rate and compile success rate decrease versus originally reported values
- **Because** (reader): reader inference: validation that runs only modified tests or ignores suspicious patches counts plausible but incorrect patches as solved; the original settings were easier than the replication
- **Unless:** relative tool ordering can survive (LearnByInteract stays above OpenHands, swe_bench:85, :86) even as absolute rates fall
- **Stated:** implied · **Interesting:** yes — denies the default that benchmark-reported LLM effectiveness transfers; better instances are separated as relative, not absolute, results
- **Papers (2; ai4se, testing):** are_solved_issues_in_swe_bench_reall, on_the_flakiness_of_llm_generated_te
- **Instances (9):** ai4se/are_solved_issues_in_swe_bench_reall:71, ai4se/are_solved_issues_in_swe_bench_reall:72, ai4se/are_solved_issues_in_swe_bench_reall:73, ai4se/are_solved_issues_in_swe_bench_reall:74, ai4se/are_solved_issues_in_swe_bench_reall:81, ai4se/are_solved_issues_in_swe_bench_reall:82, ai4se/are_solved_issues_in_swe_bench_reall:83, ai4se/are_solved_issues_in_swe_bench_reall:84, testing/on_the_flakiness_of_llm_generated_te:112
- **Counter-instances (2):** ai4se/are_solved_issues_in_swe_bench_reall:85, ai4se/are_solved_issues_in_swe_bench_reall:86
- **Stem:** learning-based | subject-model | effectiveness

```
worsen(effectiveness(outputs, approach=learning-based), strictness(validation))
  because: ~ validation(tests) = modified-only → patches(plausible, incorrect) = counted-solved → resolved(plausible) > resolved(correct)
  unless: comparison = relative-ranking
```

### LB-A-12

When an LLM judge filters or steers candidate patches or programs, the success rate among what survives rises with the judge's strictness and grounding (a specification, per-point postconditions), because a grounded, strict judge rejects more wrong candidates than right ones and can replace missing test oracles, unless the judge is ungrounded (a trajectory-only binary judgment gives no gain) or one counts absolute yield, which falls as correct candidates are discarded and flattens as attempts grow.

- **When:** an LLM-as-judge sits between generation and acceptance, as a filter (validation, abstention) or as feedback (NL postcondition judgments)
- **Techniques:** llm-as-judge
- **Effect:** raises fail-to-pass/accept rate among surviving candidates and problems solved with judge feedback; lowers overall pass-and-validation yield
- **Because** (paper): The judge's score makes precision increase with threshold, so a stricter filter keeps a higher share of correct patches; rejecting more patches reduces the bugs that keep any patch, so overall resolution falls and pass@k flattens; NL postcondition judgments replace test failures, so no test oracle is needed.
- **Unless:** the judge has no specification and gives a binary verdict (about +0 pp), the measure is absolute resolution (-2 to -7 pp as aggressiveness rises), or reproduction tests already make patches plausible (sanitizer bugs, where the unspecified validator did as well and more attempts outstrip validation)
- **Stated:** implied · **Interesting:** yes — Separates two readings of 'effectiveness' that point in opposite directions (conditional success rate up, absolute yield down) and names grounding as the condition for any gain; links two areas where an LLM judge stands in for tests.
- **Papers (2; ai4se, formal):** abstain_and_validate_a_dual_llm_poli, hoareprompt_structural_reasoning_abo
- **Instances (14):** ai4se/abstain_and_validate_a_dual_llm_poli:95, ai4se/abstain_and_validate_a_dual_llm_poli:103, ai4se/abstain_and_validate_a_dual_llm_poli:109, ai4se/abstain_and_validate_a_dual_llm_poli:111, ai4se/abstain_and_validate_a_dual_llm_poli:113, ai4se/abstain_and_validate_a_dual_llm_poli:117, ai4se/abstain_and_validate_a_dual_llm_poli:141, ai4se/abstain_and_validate_a_dual_llm_poli:148, ai4se/abstain_and_validate_a_dual_llm_poli:149, formal/hoareprompt_structural_reasoning_abo:165, formal/hoareprompt_structural_reasoning_abo:168, formal/hoareprompt_structural_reasoning_abo:171, formal/hoareprompt_structural_reasoning_abo:174, formal/hoareprompt_structural_reasoning_abo:176
- **Counter-instances (9):** ai4se/abstain_and_validate_a_dual_llm_poli:166, ai4se/abstain_and_validate_a_dual_llm_poli:168, ai4se/abstain_and_validate_a_dual_llm_poli:169, ai4se/abstain_and_validate_a_dual_llm_poli:170, ai4se/abstain_and_validate_a_dual_llm_poli:119, ai4se/abstain_and_validate_a_dual_llm_poli:147, ai4se/abstain_and_validate_a_dual_llm_poli:125, ai4se/abstain_and_validate_a_dual_llm_poli:160, ai4se/abstain_and_validate_a_dual_llm_poli:162
- **Stem:** learning-based | proposed | effectiveness

```
improve(effectiveness(fail-to-pass, approach=learning-based, judge=grounded, strictness=raised))
  because: score(correctness, positive-judgment) = exp-mean-logprob → precision(validation) increasing-in threshold → fail-to-pass(k=1) increasing-in threshold
  because: judgments(correctness, nsp) = replace-test-failures → oracles(tests) = not-needed → correct(codegen) increasing
  unless: specification(judge) = absent | measure = pass-and-validation → count(patches, rejected) increasing-in aggressiveness → pass-and-validation decreasing-in aggressiveness
```

### LB-A-7

When a learning-based tool's quality is estimated offline, by replaying history (backtest) or by reporting the configuration that scored best on the test data, the estimate misstates what deployment delivers, because a replay cannot see how people react to changed recommendations and test-set selection is optimistic, unless the configuration is chosen on held-out validation data, which kept the gap within a few points in most cells.

- **When:** offline (backtest or oracle-selected) quality is compared with online (A/B) or validation-selected quality
- **Techniques:** learned-ranking, probing
- **Effect:** offline effect sizes are larger than deployable ones (backtest -21 pp accuracy vs A/B -5 pp; best layer up to 12.5 pp above the validation-chosen layer)
- **Because** (reader): Replay holds human behaviour fixed and oracle selection fits noise in the test set, so both exaggerate the magnitude of effects relative to deployment.
- **Unless:** layer choice by held-out validation data, where the gap rarely exceeded 7 pp
- **Stated:** implied · **Interesting:** yes — Denies the default belief that offline replay or best-configuration numbers are faithful proxies for deployment: the only paper with both a backtest and an A/B test found a large discrepancy. Spans human and ai4se.
- **Papers (2; ai4se, human):** improving_code_reviewer_recommendation, on_llms_internal_representation_of_c
- **Instances (6):** human/improving_code_reviewer_recommendation:101, human/improving_code_reviewer_recommendation:89, human/improving_code_reviewer_recommendation:94, ai4se/on_llms_internal_representation_of_c:162, ai4se/on_llms_internal_representation_of_c:163, ai4se/on_llms_internal_representation_of_c:164
- **Counter-instances (3):** human/improving_code_reviewer_recommendation:87, human/improving_code_reviewer_recommendation:93, ai4se/on_llms_internal_representation_of_c:161
- **Stem:** learning-based | proposed | quality

```
worsen(validity(quality-estimate, approach=learning-based, evaluation=offline))
  because: behaviour(human, replay) = fixed → effect(recommendation-change, backtest) > effect(recommendation-change, abtest)
  because: selection(configuration, test-set) = oracle → accuracy(selection, reported) > accuracy(selection, deployable)
  unless: selection(layer, validation-set) = held-out → gap(accuracy) ≤ 7 pp
```

### DT-8

When a baseline technique is run on a stronger substrate (a better base fuzzer, a more capable LLM), it does better, so comparisons against the original implementation mix up the idea with its engine. The likely reason is that a technique's gain adds to the capability of the engine underneath, unless the stronger substrate is paired with an oracle that stays unfaithful.

- **When:** the same technique idea is hosted on engines or models of different strength
- **Techniques:** greybox-fuzzing, test-based-validation
- **Effect:** the re-hosted baseline gets higher coverage or correctness (DICE on MultiFuzz beats original DICE on every binary; test-driven refinement gains grow with model capability)
- **Because** (reader): reader: the technique's contribution composes with the underlying engine's search or generation capability, so a stronger engine raises the baseline's ceiling
- **Unless:** a weaker substrate is paired with an unfaithful oracle (llama70b testing variant ≤ baseline); the evidence for model capability rests on two models and is hedged
- **Stated:** reader · **Interesting:** yes — Holds in two areas and carries a lesson for evaluation: a baseline's measured strength depends on its host, so reimplementing a baseline on the proposed tool's substrate (as dyma does) is needed for a fair comparison.
- **Papers (2; formal, testing):** dyma_fuzz_dynamic_direct_memory_acce, hoareprompt_structural_reasoning_abo
- **Instances (2):** testing/dyma_fuzz_dynamic_direct_memory_acce:137, formal/hoareprompt_structural_reasoning_abo:172
- **Counter-instances (1):** formal/hoareprompt_structural_reasoning_abo:169
- **Stem:** dynamic-testing | baseline | effectiveness

```
improve(effectiveness(baseline, approach=dynamic-testing, substrate=stronger))
  because: gain(technique) composes-with capability(substrate) → effectiveness(baseline) increasing-in capability(substrate)
  unless: oracle(feedback) = unfaithful
```

### LB-A-14

When an LLM-based prover is given oracle help (perfect premises, perfect decomposition) or a combination is declared the upper bound, the result is not a ceiling: oracle inputs do not always prove more theorems and further combinations beat the claimed bound, because budgeted stochastic search follows different paths under different inputs, so pooling runs gains more than improving any single run's inputs.

- **When:** an oracle-assisted or 'upper-bound' configuration of a learning-based prover is compared with the default and with unions of configurations
- **Techniques:** proof-synthesis
- **Effect:** oracle inputs raise success on some datasets but not all; unions of versions exceed both the default and the claimed bound
- **Because** (reader): With a fixed sampling or search budget, each input variant reaches a different subset of theorems, so union across variants adds coverage while a better input can lose theorems the default happened to find.
- **Unless:** none observed beyond single datasets; the union of versions is a post-hoc selection, not a single deployable run
- **Stated:** implied · **Interesting:** yes — Denies the default belief that oracle inputs or a declared union give an upper bound: Cobblestone's perfect-premise plus perfect-decomposition run proves fewer CoqGym theorems than the default, and ProofCoop's claimed bound is beaten by union plus sharing and by a Diva with ProofCoop.
- **Papers (2; formal):** cobblestone_a_divide_and_conquer_app, proofcoop_collaborative_automated_fo
- **Instances (7):** formal/cobblestone_a_divide_and_conquer_app:202, formal/cobblestone_a_divide_and_conquer_app:204, formal/cobblestone_a_divide_and_conquer_app:205, formal/cobblestone_a_divide_and_conquer_app:208, formal/cobblestone_a_divide_and_conquer_app:210, formal/proofcoop_collaborative_automated_fo:89, formal/proofcoop_collaborative_automated_fo:90
- **Counter-instances (1):** formal/cobblestone_a_divide_and_conquer_app:207
- **Stem:** learning-based | proposed | effectiveness

```
bound(effectiveness(proofs, approach=learning-based, oracle=on))
  because: budget(search) = fixed ∧ sampling = stochastic → theorems(proved, variant) = variant-specific-subset → theorems(union-of-variants) > max(theorems(variant))
  unless: interaction(oracle, human) = real
```


## T4 Irrelevant surface cues sway learned judgement

### NH-12

When LLMs judge people or artifacts (selecting and assigning developers from GitHub profiles, rating generated code and summaries), their judgments depend on attributes of the source that should be irrelevant (pronouns and country; whether the code or summary was written by an LLM or a human), but the direction is consistent across models only for the most salient source attributes (they/them candidates disfavoured by all three models; LLM-generated code overrated by all judges), because models reproduce learned associations with surface signals of the source rather than assessing merit, unless the attribute is one where models disagree (she/her, country), where the sign flips by model and prose claims outrun the regressions.

- **When:** LLMs acting as evaluators or allocators over inputs that carry source identity
- **Techniques:** none (phenomenon: LLM judgment bias), LLM team formation and task allocation, LLM-as-a-judge for code generation and summarisation
- **Effect:** source-dependent bias; consistent sign for they/them and LLM-authored code, inconsistent sign for she/her and country
- **Because** (reader): learned associations between surface source signals (pronouns, nationality, generation style) and quality/role drive judgments independent of merit
- **Unless:** attributes where models disagree (she/her significant for GPT only; country direction contradicted by tables); stylistic dimensions where human text is overrated (fluency) or all text underrated (conciseness)
- **Stated:** implied · **Interesting:** yes — Cross-area pattern (human, ai4se): LLM judges of people and of code share source-identity bias; the condition separates consistent from model-specific directions and flags prose claims contradicted by tables.
- **Papers (2; ai4se, human):** on_the_effectiveness_of_llm_as_a_jud, once_upon_a_team_investigating_bias_in
- **Instances (7):** human/once_upon_a_team_investigating_bias_in:30, human/once_upon_a_team_investigating_bias_in:37, human/once_upon_a_team_investigating_bias_in:38, human/once_upon_a_team_investigating_bias_in:74, human/once_upon_a_team_investigating_bias_in:75, ai4se/on_the_effectiveness_of_llm_as_a_jud:115, ai4se/on_the_effectiveness_of_llm_as_a_jud:136
- **Counter-instances (5):** human/once_upon_a_team_investigating_bias_in:39, human/once_upon_a_team_investigating_bias_in:40, human/once_upon_a_team_investigating_bias_in:41, human/once_upon_a_team_investigating_bias_in:42, ai4se/on_the_effectiveness_of_llm_as_a_jud:132
- **Stem:** none | none | fairness

```
depend(judgment(llm, approach=none), identity(source))
  because: association(surface-signal, quality|role) = learned → judgment(llm) depends-on identity(source)
  unless: attribute ∈ {she-her, country} → sign(bias) depends-on model
```

### LB-B-11

When commercial LLMs make team-selection, task-assignment or code-quality judgements, their decisions depend on task-irrelevant identity and provenance cues in every vendor tested (they/them pronouns are strongly disfavoured, country shifts selection, human-written code is underrated relative to the judge's own), but LLM judges show no preference for their own code over other LLMs' code, because the cues that move the decision separate rare or out-of-distribution sources from common ones, not one model from another.

- **When:** LLM acts as decision-maker or judge on items that carry identity or provenance cues
- **Techniques:** commercial-llm, llm-as-judge
- **Effect:** selection odds, assignment risk and judged quality depend on pronouns, country, and human-vs-LLM authorship
- **Because** (reader): reader inference: models favour inputs resembling high-frequency patterns in their training or generation distribution (model-style code over human code; common pronoun forms over they/them); no paper states a mechanism
- **Unless:** comparison is between LLM generators (no self-bias except GPT-4 on generation: on_the_effectiveness:109, :123); direction is not uniformly against minorities (any-all 2.5× under GPT, she-they 2.9× under Claude; NG/BR/IN favoured over US/UK)
- **Stated:** reader · **Interesting:** yes — denies the default that LLMs are neutral decision-makers, across three vendors and two areas; the null self-bias results supply the separating condition (human vs model provenance, not model vs model)
- **Papers (2; ai4se, human):** on_the_effectiveness_of_llm_as_a_jud, once_upon_a_team_investigating_bias_in
- **Instances (44):** human/once_upon_a_team_investigating_bias_in:45, human/once_upon_a_team_investigating_bias_in:46, human/once_upon_a_team_investigating_bias_in:47, human/once_upon_a_team_investigating_bias_in:49, human/once_upon_a_team_investigating_bias_in:50, human/once_upon_a_team_investigating_bias_in:51, human/once_upon_a_team_investigating_bias_in:52, human/once_upon_a_team_investigating_bias_in:53, human/once_upon_a_team_investigating_bias_in:54, human/once_upon_a_team_investigating_bias_in:55, human/once_upon_a_team_investigating_bias_in:56, human/once_upon_a_team_investigating_bias_in:57, human/once_upon_a_team_investigating_bias_in:58, human/once_upon_a_team_investigating_bias_in:59, human/once_upon_a_team_investigating_bias_in:60, human/once_upon_a_team_investigating_bias_in:61, human/once_upon_a_team_investigating_bias_in:62, human/once_upon_a_team_investigating_bias_in:63, human/once_upon_a_team_investigating_bias_in:65, human/once_upon_a_team_investigating_bias_in:67, human/once_upon_a_team_investigating_bias_in:68, human/once_upon_a_team_investigating_bias_in:69, human/once_upon_a_team_investigating_bias_in:70, human/once_upon_a_team_investigating_bias_in:79, human/once_upon_a_team_investigating_bias_in:80, human/once_upon_a_team_investigating_bias_in:81, human/once_upon_a_team_investigating_bias_in:83, human/once_upon_a_team_investigating_bias_in:84, human/once_upon_a_team_investigating_bias_in:88, human/once_upon_a_team_investigating_bias_in:89, human/once_upon_a_team_investigating_bias_in:90, human/once_upon_a_team_investigating_bias_in:92, human/once_upon_a_team_investigating_bias_in:93, human/once_upon_a_team_investigating_bias_in:94, human/once_upon_a_team_investigating_bias_in:95, human/once_upon_a_team_investigating_bias_in:96, human/once_upon_a_team_investigating_bias_in:97, human/once_upon_a_team_investigating_bias_in:99, human/once_upon_a_team_investigating_bias_in:100, human/once_upon_a_team_investigating_bias_in:102, human/once_upon_a_team_investigating_bias_in:103, human/once_upon_a_team_investigating_bias_in:104, human/once_upon_a_team_investigating_bias_in:109, ai4se/on_the_effectiveness_of_llm_as_a_jud:117
- **Counter-instances (5):** ai4se/on_the_effectiveness_of_llm_as_a_jud:109, ai4se/on_the_effectiveness_of_llm_as_a_jud:123, human/once_upon_a_team_investigating_bias_in:48, human/once_upon_a_team_investigating_bias_in:64, human/once_upon_a_team_investigating_bias_in:66
- **Stem:** learning-based | subject-model | fairness

```
depends-on(decision(selection|assignment|judgement, approach=learning-based), cue(identity|provenance))
  because: ~ frequency(pattern, training) = low → preference(llm, pattern) = lower → odds(selection, pronouns=they-them) < 1× ∧ bias(judge, generator=Human) < 0
  unless: comparison = generator=own vs generator=other-llm
```

### LB-B-12

When LLMs rank or rate code artefacts, task-irrelevant presentation features (position in a list, which rating criterion is asked) shift their outputs, and the order effect grows with the number of items in context, because each item's position carries more weight as more methods share the prompt; the effect persists for short lists and renamed methods, so it is not data leakage.

- **When:** LLM judges or ranks several items presented together
- **Techniques:** commercial-llm, llm-as-judge
- **Effect:** accuracy gap between best and worst ordering increases with segment size; GPT-family judges overrate content adequacy and underrate conciseness
- **Because** (paper): methods in the prompt increase with segment size, and the weight of order increases with methods, so the accuracy gap increases (order_matters:96)
- **Unless:** fewer items reduce but do not remove the bias (order_matters:93); renaming does not remove it (order_matters:112)
- **Stated:** implied · **Interesting:** yes — paper mechanism plus a condition (items in context) that scales the bias; the renaming result denies the leakage explanation
- **Papers (2; ai4se):** on_the_effectiveness_of_llm_as_a_jud, order_matters_an_empirical_study_on
- **Instances (6):** ai4se/order_matters_an_empirical_study_on:96, ai4se/order_matters_an_empirical_study_on:98, ai4se/order_matters_an_empirical_study_on:108, ai4se/order_matters_an_empirical_study_on:112, ai4se/on_the_effectiveness_of_llm_as_a_jud:129, ai4se/on_the_effectiveness_of_llm_as_a_jud:134
- **Counter-instances (1):** ai4se/order_matters_an_empirical_study_on:93
- **Stem:** learning-based | subject-model | fairness

```
worsen(fairness(judgement|ranking, approach=learning-based), items(context))
  because: methods(prompt) increasing-in segment-size → weight(order, methods) increasing-in methods → gap(accuracy, fl) increasing-in segment-size
  assume: gap(accuracy) independent-of names
  unless: gap(accuracy) > 0 | segment-size = small
```

### LB-B-14

When a baseline's decision is keyed to surface patterns of the data it was built or tuned on (hand-written prelude rules, learned rankers, intrinsic confidence), its accuracy collapses under a shift in format or benchmark (a while(!false) variant defeats the rules; rankers fall to random on BigCodeBench), because the surface patterns do not carry over while the underlying semantics do.

- **When:** evaluation input differs in surface form or benchmark from the method's development data
- **Techniques:** deobfuscation, llm-confidence, learned-ranking
- **Effect:** accuracy depends on code format / benchmark; at or below random under shift
- **Because** (reader): rules and learned rankers encode syntactic or distributional regularities of their development data; semantically equivalent but syntactically different inputs, or harder benchmarks, break those regularities
- **Unless:** evaluation on the benchmark family used in development (HumanEval), or fewer candidates to rank (on_llms:184)
- **Stated:** implied · **Interesting:** yes — reader mechanism unifying rule-based and learned baselines: brittleness comes from surface-keyed decisions, with a condition (format/benchmark shift)
- **Papers (2; ai4se):** cascade_llm_powered_javascript_deobf, on_llms_internal_representation_of_c
- **Instances (5):** ai4se/cascade_llm_powered_javascript_deobf:72, ai4se/on_llms_internal_representation_of_c:141, ai4se/on_llms_internal_representation_of_c:172, ai4se/on_llms_internal_representation_of_c:174, ai4se/on_llms_internal_representation_of_c:184
- **Stem:** learning-based | baseline | quality

```
worsen(quality(accuracy, approach=learning-based, keyed-to=surface), shift(format|benchmark))
  because: rules(detection) = syntactic → variants(equivalent, syntactic) defeat rules → accuracy depends-on code-format
  because: regularities(ranker, development-data) ≠ regularities(benchmark, harder) → accuracy(ranking, k=2) ≤ random
  unless: benchmark = development-family
```


## T5 Claims argued, not measured

### AV-7

When analysis tools claim generality (other inference algorithms, other PPLs, other decompilers, upgradable protocols, emerging threats), the claim rests on a narrow integration interface (JSON over HTTP, register-based SSA, proxy upgrades, whitelisting) rather than on measurement, because a small interface makes porting look cheap, unless generality is actually measured, where an exact tool can still cover fewer cases than approximate competitors.

- **When:** generality or portability statements about a proposed analysis tool
- **Techniques:** interactive-debugging, static-analysis, runtime-verification, symbolic-execution
- **Effect:** generality asserted, not evaluated
- **Because** (reader): tools are built around a narrow intermediate interface, so authors infer portability from the interface rather than testing other targets
- **Unless:** the one measured generality claim (loop types supported on C4B) shows gains over other summarizers but a loss against abstract interpreters
- **Stated:** implied · **Interesting:** yes — cross-area validity pattern: generality is argued from design in testing and formal papers alike, and the only measured case shows it is not free
- **Papers (3; formal, testing):** enforcing_control_flow_integrity_on_, online_and_interactive_bayesian_infe, precise_static_identification_of_eth
- **Instances (5):** testing/online_and_interactive_bayesian_infe:116, testing/online_and_interactive_bayesian_infe:117, testing/precise_static_identification_of_eth:88, formal/enforcing_control_flow_integrity_on_:197, formal/enforcing_control_flow_integrity_on_:198
- **Counter-instances (4):** formal/loopscc_summarizing_complex_multi_br:99, formal/loopscc_summarizing_complex_multi_br:100, formal/loopscc_summarizing_complex_multi_br:101, formal/loopscc_summarizing_complex_multi_br:106
- **Stem:** analysis-and-verification | proposed | generality

```
assert(generality(support, approach=analysis-and-verification, evidence=interface-argument))
  because: interface(integration) = narrow → porting(cost) = assumed-low → generality(support) = asserted
  unless: support(cases) < approximate-tools | generality=measured, semantics=exact
```

### AV-9

When analysis papers report that precision or recall is independent of hard constructs (inline assembly, macro-generated functions), the null rests on unsampled or unmeasured evidence, and aggregate quality stays flat because those constructs are a small share of the corpus, unless per-construct quality is measured directly.

- **When:** null claims that tool quality does not depend on rare, hard-to-handle code constructs
- **Techniques:** static-analysis, bounded-model-checking
- **Effect:** precision/recall reported as independent of the construct
- **Because** (reader): rare constructs contribute little to aggregate metrics, so failures on them do not move the average ("macros make up a small portion of most Rust codebases")
- **Unless:** per-construct precision/recall measured with an adequate sample
- **Stated:** implied · **Interesting:** yes — cross-area; denies the default reading of an independence claim as evidence of robustness: the null is explained by rarity, not by handling
- **Papers (2; formal, testing):** harnessllm_rust_verification_harness, precise_static_identification_of_eth
- **Instances (3):** testing/precise_static_identification_of_eth:61, testing/precise_static_identification_of_eth:75, formal/harnessllm_rust_verification_harness:64
- **Stem:** analysis-and-verification | proposed | quality

```
null(quality(precision|recall, approach=analysis-and-verification), construct=rare-hard)
  because: share(construct, corpus) = small → effect(construct, aggregate-quality) = small → quality(aggregate) independent-of construct
  unless: quality(per-construct) = measured
```

### AV-12

When verification-tool papers claim soundness, exact semantics or reduced misuse, the claim is argued by construction (policies equivalent to safe flows, test-derived calling scenarios, concrete interpretation) rather than proved in full or measured, because the property is taken to follow from the design, unless best-effort or unproven components are involved (heuristics under oracle manipulation, SCC orders outside the theorems), which the papers themselves flag.

- **When:** correctness, soundness or misuse claims about a proposed verification or analysis tool
- **Techniques:** runtime-verification, bounded-model-checking, symbolic-execution
- **Effect:** correctness asserted by design argument; not evaluated
- **Because** (paper): whitelisted flows are state-equivalent to EOA or read-only calls -> sound by design; test-derived scenarios are realistic -> misuse decreases (paper because lines)
- **Unless:** heuristic components (best-effort, broken by external oracle manipulation or non-standard ERC20) and proofs covering only high-order SCC reduction
- **Stated:** implied · **Interesting:** yes — denies the default belief that verification papers verify their own correctness claims; the unless lines locate exactly where the design argument stops covering the tool
- **Papers (3; formal):** enforcing_control_flow_integrity_on_, harnessllm_rust_verification_harness, loopscc_summarizing_complex_multi_br
- **Instances (5):** formal/enforcing_control_flow_integrity_on_:99, formal/enforcing_control_flow_integrity_on_:102, formal/harnessllm_rust_verification_harness:169, formal/loopscc_summarizing_complex_multi_br:74, formal/loopscc_summarizing_complex_multi_br:53
- **Stem:** analysis-and-verification | proposed | correctness

```
assert(correctness(soundness|misuse, approach=analysis-and-verification, evidence=by-construction))
  because: design(policies|scenarios|semantics) = safe-by-construction → correctness = argued
  unless: soundness = best-effort | heuristics=present, oracle-manipulation=present, theorem-coverage=partial
```

### LB-B-6

When a learning-based tool routes the LLM through an enforced, semantic workflow (fixed steps, semantic log parsing, structured summaries), its authors claim it is more explainable and maintainable than rule-based or free-form alternatives, because intermediate steps become observable and component-specific rules disappear, although no paper measured the claim with users or maintainers.

- **When:** LLM output is produced inside an enforced multi-step or semantic pipeline
- **Techniques:** deobfuscation, llm-prompting
- **Effect:** claimed higher explainability/interpretability and maintainability (asserted, unmeasured)
- **Because** (paper): semantic parsing of heterogeneous logs removes component-specific parsing rules (autodiagnose:136); an enforced workflow exposes intermediate steps (cascade:136)
- **Unless:** never evaluated with moderators, developers, or maintainers (toxicity:96, cascade:136, autodiagnose:136)
- **Stated:** implied · **Interesting:** yes — cross-area pattern with a paper mechanism; also flags a recurring unmeasured usability claim
- **Papers (3; ai4se, human):** cascade_llm_powered_javascript_deobf, llm_based_automated_diagnosis_of_int, toxicity_ahead_forecasting_conversation
- **Instances (3):** ai4se/cascade_llm_powered_javascript_deobf:136, ai4se/llm_based_automated_diagnosis_of_int:136, human/toxicity_ahead_forecasting_conversation:96
- **Stem:** learning-based | proposed | usability

```
improve(usability(explainability|maintainability, approach=learning-based, workflow=enforced))
  because: parsing(logs, heterogeneous) = semantic → rules(parsing, component-specific) = none → maintainability increasing
  because: workflow(steps) = enforced → steps(intermediate) = observable → explainability increasing
  assume: users(evaluation) = none
```

### LB-A-3

When learning-based tools are presented as saving human effort, the saving is mostly asserted, not measured, and where it was measured in a live A/B test a more accurate reviewer recommender left human review time and time spent unchanged, because a tool that improves one decision (who reviews, which patch, which solution) does not shorten the human task that follows, unless the tool changes what or whom the human process involves (choosing which traces a user inspects, adding a bystander reviewer), where effort or time did fall.

- **When:** a learning-based tool claims to reduce human effort or time
- **Techniques:** learned-ranking, llm-fine-tuning, llm-as-judge, deobfuscation, probing, proof-synthesis, llm-prompting, spec-inference
- **Effect:** human effort/time: asserted reduction; measured parity when only decision accuracy improves; measured reduction when the tool restructures the human's task
- **Because** (reader): Improving the accuracy of a choice does not change how long the chosen human's own work takes; only redefining what the human must inspect or who is involved changes it.
- **Unless:** the tool picks the items the human inspects (ARTEMIS: validation queries grow as log n) or changes the reviewer pool (bystander recommendation cut time in review by 11.6%)
- **Stated:** reader · **Interesting:** yes — Denies the default belief that better tool accuracy saves developer time: the only field A/B test in the stem found parity (and failed to replicate a prior 14-21% cycle-time reduction), while six of eight papers only assert effort savings. Spans all four areas, and the condition (the tool restructures the human task) matches the cases where effort did fall.
- **Papers (8; ai4se, formal, human, testing):** abstain_and_validate_a_dual_llm_poli, automating_requirements_formalizatio, cascade_llm_powered_javascript_deobf, e_test_e_er_improving_test_suites, improving_code_reviewer_recommendation, llm_based_automated_diagnosis_of_int, on_llms_internal_representation_of_c, proofcoop_collaborative_automated_fo
- **Instances (11):** human/improving_code_reviewer_recommendation:63, human/improving_code_reviewer_recommendation:64, human/improving_code_reviewer_recommendation:68, human/improving_code_reviewer_recommendation:69, human/improving_code_reviewer_recommendation:70, testing/e_test_e_er_improving_test_suites:170, ai4se/abstain_and_validate_a_dual_llm_poli:203, ai4se/cascade_llm_powered_javascript_deobf:87, ai4se/on_llms_internal_representation_of_c:204, formal/proofcoop_collaborative_automated_fo:219, ai4se/llm_based_automated_diagnosis_of_int:89
- **Counter-instances (4):** human/improving_code_reviewer_recommendation:111, formal/automating_requirements_formalizatio:88, formal/automating_requirements_formalizatio:160, ai4se/llm_based_automated_diagnosis_of_int:82
- **Stem:** learning-based | proposed | cost

```
bound(effort(human-task, approach=learning-based, tool-accuracy=raised))
  because: accuracy(decision, tool) increasing → choice(human) = changed → duration(human-task) = unchanged
  unless: selection(items, inspected-by-human) = tool-chosen → effort(validation, traces) ∝ log-n
```

### NH-3

When analysis and testing tools need a cheap filter over a large space (MMIO writes, DeFi transactions, Rust harness inputs, candidate patches), they ground it in a skewed prevalence of the domain (valid RAM pointers are almost never written to MMIO; the small share of external transactions carries most non-trivial control flow; Rust projects ship rich tests; suspicious patches mostly add rather than omit changes), because a strong skew lets a simple rule separate the rare interesting cases, unless the skew is only asserted or measured on one platform, in which case the filter's soundness is unverified elsewhere.

- **When:** tool design over a large input space where a population statistic justifies a filter or premise
- **Techniques:** none (phenomenon: domain population statistics), firmware fuzzing, smart-contract control-flow integrity, LLM harness generation for Rust verification, benchmark patch validation
- **Effect:** a strongly skewed share (near-zero or concentrated) is observed and used as the premise of a heuristic
- **Because** (reader): when the target class is rare or concentrated, a structural signal (value range, actor type, presence of tests, added-vs-omitted changes) partitions cases with few errors
- **Unless:** skew asserted without measurement (Rust test quality, macro share) or measured on one MCU family
- **Stated:** implied · **Interesting:** yes — Cross-area pattern (testing, formal, ai4se): heuristics in unrelated tools rest on the same kind of prevalence skew, and the condition exposes where that premise is untested.
- **Papers (4; ai4se, formal, testing):** are_solved_issues_in_swe_bench_reall, dyma_fuzz_dynamic_direct_memory_acce, enforcing_control_flow_integrity_on_, harnessllm_rust_verification_harness
- **Instances (6):** testing/dyma_fuzz_dynamic_direct_memory_acce:52, formal/enforcing_control_flow_integrity_on_:66, formal/enforcing_control_flow_integrity_on_:71, formal/harnessllm_rust_verification_harness:50, formal/harnessllm_rust_verification_harness:166, ai4se/are_solved_issues_in_swe_bench_reall:122
- **Stem:** none | none | prevalence

```
justify(heuristic(filter, approach=none), share(target, population) = skewed)
  because: share(target, population) ≈ 0 | concentrated → error(filter, structural-signal) = low
  unless: share(target, population) = asserted | platform = single
```


## T6 Every gain is a trade-off at a threshold

### AV-8

When an analysis relaxes exactness (abstract instead of concrete loop semantics, best-effort pruning heuristics, method-level instead of class-level impact), it gains breadth, fewer false alarms or speed because fewer paths, flows or specs need exact treatment, but it loses guarantees (lower summarization accuracy, best-effort soundness, missed violations), unless the relaxed cases are rare in the corpus, where the guarantee loss is negligible.

- **When:** choice between exact and approximate program semantics or impact analysis
- **Techniques:** symbolic-execution, runtime-verification
- **Effect:** approximation: breadth, false-positive rate and speed improve; accuracy and soundness degrade
- **Because** (paper): approximate semantics -> wider loop support; heuristics prune read-only, ERC20 and independent invocations -> fewer non-trivial flows -> lower FP; method-level granularity -> partial soundness -> detections < 100% (paper because lines)
- **Unless:** relaxed cases rare: FineMOP misses at most 0.32% of new violations and 0 after inspection for the safe variants; exact summarization itself fails on nonlinear loops it cannot represent
- **Stated:** implied · **Interesting:** yes — a precision-for-breadth trade stated as one law across three techniques, with a condition (rarity of relaxed cases) under which the cost disappears; mechanism from paper because lines
- **Papers (3; formal):** enforcing_control_flow_integrity_on_, fine_grained_analyses_for_evolution_, loopscc_summarizing_complex_multi_br
- **Instances (9):** formal/loopscc_summarizing_complex_multi_br:106, formal/loopscc_summarizing_complex_multi_br:79, formal/loopscc_summarizing_complex_multi_br:90, formal/loopscc_summarizing_complex_multi_br:111, formal/loopscc_summarizing_complex_multi_br:53, formal/enforcing_control_flow_integrity_on_:138, formal/enforcing_control_flow_integrity_on_:102, formal/fine_grained_analyses_for_evolution_:180, formal/fine_grained_analyses_for_evolution_:45
- **Counter-instances (3):** formal/fine_grained_analyses_for_evolution_:191, formal/fine_grained_analyses_for_evolution_:185, formal/fine_grained_analyses_for_evolution_:200
- **Stem:** analysis-and-verification | proposed | quality

```
tradeoff(quality(accuracy|soundness, approach=analysis-and-verification, exactness=relaxed), support|fp-rate|time)
  because: semantics|granularity = approximate → paths|flows|specs(exact) = fewer → support increasing-in approximation, fp-rate decreasing-in pruning
  because: approximation = on → soundness = partial → accuracy|detections < exact
  unless: loss(guarantee) = negligible | relaxed-cases=rare
```

### LB-B-18

When a learned probe or model edit is fitted closely to one distribution or perturbation type, it gains in-distribution robustness or accuracy at the price of generalization (higher fold-to-fold variance; lower cross-type gain; decline after an intermediate regularization or step budget), because specialisation captures in-distribution nuances that other folds or perturbation types do not share, and unconstrained editing overfits.

- **When:** fitting source or edit is specific to one distribution/perturbation type, or editing runs without early stopping
- **Techniques:** model-editing, probing
- **Effect:** in-distribution robustness/accuracy up; cross-distribution gain lower; variance higher; non-monotone in regularization strength
- **Because** (paper): in-distribution stimuli capture nuances, raising accuracy with specialisation but leaving some folds with nuances missed, so SD rises (on_llms:177); overfitting of the edit increases with steps (creme:101)
- **Unless:** cross-type gain stays positive (18% vs 24%, creme:85), so specialisation does not fully overfit
- **Stated:** implied · **Interesting:** yes — paper mechanism for a specialisation-versus-generalization trade-off seen in two different learned interventions (probing and editing)
- **Papers (2; ai4se):** creme_robustness_enhancement_of_code, on_llms_internal_representation_of_c
- **Instances (5):** ai4se/on_llms_internal_representation_of_c:177, ai4se/creme_robustness_enhancement_of_code:85, ai4se/creme_robustness_enhancement_of_code:101, ai4se/creme_robustness_enhancement_of_code:103, ai4se/creme_robustness_enhancement_of_code:110
- **Stem:** learning-based | proposed | robustness

```
trade(robustness(in-distribution, approach=learning-based), generality(cross-distribution), specialisation)
  because: nuances(stimuli, in-distribution) = captured → accuracy increasing-in specialisation → sd(accuracy) increasing-in specialisation
  because: steps(editing) = unbounded → overfitting(edit) increasing-in steps → grir(perturbed) decreasing
  unless: grir(cross-type) > 0
```

### LB-A-5

When a learning-based classifier or LLM judge is operated through a threshold or aggressiveness setting, raising it buys precision and a lower false-positive rate at the price of recall, because higher confidence scores separate true positives better while discarding borderline true cases, unless the scorer is given extra grounding (a specification), which shifts the whole trade-off curve rather than moving along it.

- **When:** quality is read at a chosen operating point (threshold theta, percentile p75/p90, binary judgment)
- **Techniques:** llm-prompting, llm-as-judge
- **Effect:** precision up and FPR down as the threshold rises; recall down
- **Because** (paper): The judge's score (exp mean log-prob of a positive judgment) makes more predictions distinguishable at high scores, so precision increases with threshold, while stricter thresholds reject patches or conversations that should have been kept.
- **Unless:** a specification-grounded validator has lower FPR than the ungrounded one at every threshold (0.28 vs 0.64 binary, 0.13 vs 0.15 p75, 0.04 vs 0.06 p90); a structural prompt can raise TPR and FPR together against a lenient baseline
- **Stated:** yes · **Interesting:** yes — The precision/recall trade-off itself is textbook, but the stem adds a paper-stated mechanism, holds across human, ai4se and formal, and the condition (grounding moves the curve, a threshold only moves along it) says which lever changes the direction. Also explains the one threshold (theta=0.1) where a recall-heavy baseline wins.
- **Papers (3; ai4se, formal, human):** abstain_and_validate_a_dual_llm_poli, hoareprompt_structural_reasoning_abo, toxicity_ahead_forecasting_conversation
- **Instances (7):** human/toxicity_ahead_forecasting_conversation:58, ai4se/abstain_and_validate_a_dual_llm_poli:74, ai4se/abstain_and_validate_a_dual_llm_poli:79, ai4se/abstain_and_validate_a_dual_llm_poli:81, ai4se/abstain_and_validate_a_dual_llm_poli:82, ai4se/abstain_and_validate_a_dual_llm_poli:83, formal/hoareprompt_structural_reasoning_abo:91
- **Counter-instances (5):** human/toxicity_ahead_forecasting_conversation:56, human/toxicity_ahead_forecasting_conversation:34, ai4se/abstain_and_validate_a_dual_llm_poli:171, formal/hoareprompt_structural_reasoning_abo:96, formal/hoareprompt_structural_reasoning_abo:97
- **Stem:** learning-based | proposed | quality

```
improve(precision(classification, approach=learning-based, threshold=raised))
  because: score(correctness, positive-judgment) = exp-mean-logprob → share(scores, distinguishable) increasing-in score → precision increasing-in threshold
  because: count(true-cases, discarded) increasing-in threshold → recall decreasing-in threshold
  unless: specification(validation) = provided → fpr(validation) decreasing at every threshold
```

### LB-A-8

When a learned model is re-tuned toward a secondary objective (spreading reviewer workload, fine-tuning a classifier for overall alignment), quality on the primary or minority target drops, because re-ranking or fine-tuning moves decisions away from the candidates the original model rated best, unless the change only reorders a fixed candidate set or a guardrail caps the loss.

- **When:** a learned ranker or classifier is optimised for a second objective besides its original accuracy target
- **Techniques:** learned-ranking, llm-fine-tuning
- **Effect:** lowers top-k accuracy or the F1/recall of the de-prioritised class while the secondary objective or overall score improves
- **Because** (reader): Any objective other than accuracy moves some top decisions away from the most likely correct candidate, so accuracy on that target must fall unless those decisions are constrained.
- **Unless:** the re-ranking only permutes the same top-5 set (top-5 accuracy unchanged by construction) or a guardrail bounds the drop (RevRecWL stayed above the -21 pp / -20 pp guardrails)
- **Stated:** implied · **Interesting:** yes — The same trade-off appears in two areas (human-facing recommendation and test-scenario classification), and both papers treat the loss as an accepted cost (a guardrail; 'an acceptable consequence') rather than a failure. The condition (reordering a fixed set) explains the one instance with no loss.
- **Papers (2; human, testing):** e_test_e_er_improving_test_suites, improving_code_reviewer_recommendation
- **Instances (9):** human/improving_code_reviewer_recommendation:87, human/improving_code_reviewer_recommendation:88, human/improving_code_reviewer_recommendation:90, human/improving_code_reviewer_recommendation:91, human/improving_code_reviewer_recommendation:93, human/improving_code_reviewer_recommendation:95, human/improving_code_reviewer_recommendation:96, testing/e_test_e_er_improving_test_suites:80, testing/e_test_e_er_improving_test_suites:71
- **Counter-instances (1):** human/improving_code_reviewer_recommendation:98
- **Stem:** learning-based | proposed | quality

```
worsen(quality(accuracy, approach=learning-based, secondary-objective=on))
  because: objective(ranking) ≠ accuracy → share(top-decisions, most-likely-candidate) decreasing → accuracy(top-k) decreasing
  unless: set(candidates, top5) = fixed → accuracy(top5) = unchanged | guardrail(accuracy-drop) = enforced
```

### LB-B-4

When a learning-based tool filters its candidates (by classifier precision or a percentile cutoff), the size of what it hands to humans or adds to a suite is set by the filter's selectivity, because a looser cutoff or lower precision admits more redundant or low-value items (a p75 cutoff doubles bugs for inspection versus p90; high already-tested precision keeps an augmented suite bounded).

- **When:** tool output passes through a learned filter or percentile threshold before reaching users or suites
- **Techniques:** llm-fine-tuning, llm-as-judge, test-generation
- **Effect:** output volume increases as filter selectivity decreases
- **Because** (paper): share of redundant added tests decreases with classification precision, so the augmented suite stays bounded (e_test:177); lowering the percentile cutoff retains more candidates (abstain:136)
- **Unless:** none stated; e_test:177 infers boundedness from F1 without reporting suite size
- **Stated:** implied · **Interesting:** yes — cross-area mechanism: scale of LLM output is a tunable precision/volume trade-off, not a fixed property of the tool
- **Papers (3; ai4se, testing):** abstain_and_validate_a_dual_llm_poli, citywalk_enhancing_llm_based_c_unit, e_test_e_er_improving_test_suites
- **Instances (3):** testing/e_test_e_er_improving_test_suites:177, ai4se/abstain_and_validate_a_dual_llm_poli:136, testing/citywalk_enhancing_llm_based_c_unit:113
- **Stem:** learning-based | proposed | scale

```
increase(size(output, approach=learning-based), decreasing-in selectivity(filter))
  because: share(tests, added, redundant) decreasing-in precision → size(test-suite, augmented) ≤ bounded
  because: cutoff(percentile) = lower → count(bugs, retained) = higher
```

### DT-3

When a dynamic tester splits its budget between learning or sampling the subject and exploiting what it learned, effectiveness peaks at a middle split that depends on the subject. Learning time comes out of guided search, so training for longer, later, or not at all shifts results. Too little sampling misses the rare failures of stable subjects, unless the subject fails often enough that sparse sampling already exposes them.

- **When:** fixed campaign budget shared between a learning/sampling phase and a guided phase
- **Techniques:** greybox-fuzzing, simulation-testing
- **Effect:** coverage or failures found are non-monotone in the learning/sampling budget; the best setting depends on the subject
- **Because** (paper): stability(ads) = high → information(search-space, sampling) increasing-in sampling → failures increasing-in sampling; training(phase) = disabled → time(guided-phase) = 24 h → coverage > original
- **Unless:** the subject fails often (Transfuser works best with 10x1, 10x2), so extra sampling adds cost without new failures; or the learned pattern transfers from another program, so no training is needed
- **Stated:** implied · **Interesting:** yes — The subject flips the direction: more sampling helps a stable ADS and not a failure-prone one, and 1 h of training beats both 0.5 h and 2 h. Both papers give a mechanism, namely that learning is paid for in search time.
- **Papers (2; testing):** misbehavior_forecasting_for_focused, on_interaction_effects_in_greybox_fu
- **Instances (9):** testing/misbehavior_forecasting_for_focused:53, testing/misbehavior_forecasting_for_focused:45, testing/misbehavior_forecasting_for_focused:47, testing/misbehavior_forecasting_for_focused:49, testing/misbehavior_forecasting_for_focused:51, testing/on_interaction_effects_in_greybox_fu:161, testing/on_interaction_effects_in_greybox_fu:162, testing/on_interaction_effects_in_greybox_fu:166, testing/on_interaction_effects_in_greybox_fu:170
- **Stem:** dynamic-testing | proposed | effectiveness

```
improve(effectiveness(failures, approach=dynamic-testing, budget(learning) = subject-tuned))
  because: stability(subject) = high → information(search-space, sampling) increasing-in sampling → failures increasing-in sampling
  because: time(learning-phase) increasing → time(guided-phase) decreasing → coverage(fuzzing) decreasing-in excess-learning
  unless: failure-rate(subject) = high | pattern(subject) = transferable
```


## T7 Guidance fitted to the subject beats generic guidance

### DT-1

When the subject's inputs carry structure a fuzzer can exploit (many typed message kinds, strong mutator-order interactions), a fuzzer that models or learns that structure gets more coverage than one that ignores it, because structure-aware inputs reach code written for each message type and useful mutation sequences come sooner, unless the input is only used in arithmetic (ADC samples), interactions are weak, coverage is already saturated, or the baseline already models the same structure; in those cases the gain drops to null.

- **When:** subject inputs are structured (typed multi-message protocols; programs where mutator pairs interact strongly, R2_adj > 0.8)
- **Techniques:** greybox-fuzzing
- **Effect:** coverage increases over structure-blind fuzzers; the size of the gain grows with the number of message types or the strength of interaction
- **Because** (paper): messages(input, typed) = many → sections(code, per-message-type) = dedicated → coverage gain increasing-in message-types; probability(next-mutator, learned) ∝ interesting-inputs → interesting mutator sequences arrive earlier → coverage > baseline
- **Unless:** input is used only in calculations (ADC samples: guitar-pedal, oscilloscope, soldering-station); mutator interaction is weak; coverage is saturated (openssl, php, lcms, libxml); the baseline already models the DMA buffers (dice-set parity)
- **Stated:** implied · **Interesting:** yes — A subject property (input structure or interaction strength) decides whether the gain is large or null. It explains, across two fuzzers from different settings (firmware DMA, FuzzBench), why the same tool beats its baselines on some programs and ties on others.
- **Papers (2; testing):** dyma_fuzz_dynamic_direct_memory_acce, on_interaction_effects_in_greybox_fu
- **Instances (15):** testing/dyma_fuzz_dynamic_direct_memory_acce:143, testing/dyma_fuzz_dynamic_direct_memory_acce:92, testing/dyma_fuzz_dynamic_direct_memory_acce:99, testing/dyma_fuzz_dynamic_direct_memory_acce:101, testing/dyma_fuzz_dynamic_direct_memory_acce:117, testing/dyma_fuzz_dynamic_direct_memory_acce:119, testing/dyma_fuzz_dynamic_direct_memory_acce:121, testing/dyma_fuzz_dynamic_direct_memory_acce:123, testing/dyma_fuzz_dynamic_direct_memory_acce:125, testing/on_interaction_effects_in_greybox_fu:62, testing/on_interaction_effects_in_greybox_fu:69, testing/on_interaction_effects_in_greybox_fu:78, testing/on_interaction_effects_in_greybox_fu:86, testing/on_interaction_effects_in_greybox_fu:88, testing/on_interaction_effects_in_greybox_fu:90
- **Counter-instances (12):** testing/dyma_fuzz_dynamic_direct_memory_acce:95, testing/dyma_fuzz_dynamic_direct_memory_acce:97, testing/dyma_fuzz_dynamic_direct_memory_acce:103, testing/dyma_fuzz_dynamic_direct_memory_acce:109, testing/dyma_fuzz_dynamic_direct_memory_acce:115, testing/dyma_fuzz_dynamic_direct_memory_acce:127, testing/dyma_fuzz_dynamic_direct_memory_acce:135, testing/on_interaction_effects_in_greybox_fu:84, testing/on_interaction_effects_in_greybox_fu:96, testing/on_interaction_effects_in_greybox_fu:98, testing/on_interaction_effects_in_greybox_fu:104, testing/on_interaction_effects_in_greybox_fu:107
- **Stem:** dynamic-testing | proposed | effectiveness

```
improve(effectiveness(coverage, approach=dynamic-testing, structure-model=on))
  because: messages(input, typed) = many → sections(code, per-message-type) = dedicated → coverage(blocks, gain) increasing-in message-types
  because: probability(next-mutator, learned) ∝ interesting-inputs → sequences(mutator, interesting) = earlier → coverage(fuzzing) > baseline
  unless: input(adc-samples) = calculation-only | interaction(mutator-pair) = weak | coverage(fuzzing) = saturated | model(baseline, structure) = same
```

### DT-2

When a dynamic tester replaces a fixed heuristic or generic guidance with guidance taken from the subject itself (DMA pointers seen at run time, forecast misbehaviour, mutator probabilities learned per program), it finds more, because fixed heuristics carry assumptions that some subjects break (adjacent pointer pairs, sequential buffer access, proximity means risk, uniform or random mutator order), unless the subject shares the pattern the generic guidance encodes or deriving the guidance costs too much search time.

- **When:** the subject violates the assumptions built into the baseline's fixed heuristic or into generic/transferred guidance
- **Techniques:** greybox-fuzzing, simulation-testing
- **Effect:** more detections, higher yield of failures per risky point, higher coverage than heuristic, random, weighted, default-length or cross-program variants
- **Because** (paper): heuristic(dma, adjacent-pointer-pair) = unmet when an integrated interface has one pointer → DICE misses the interface; DICE's length inference stops at gaps in sparse reads; a weighted first mutator makes sequences more deterministic → less exploration → lower coverage
- **Unless:** the subject shares the generic pattern (cross-program guidance ≥ random on the nine programs that share freetype's pattern; default length wins on bloaty), or learning the guidance takes away enough search time that skipping it pays off (cross on freetype)
- **Stated:** implied · **Interesting:** yes — The mechanism is that heuristic assumptions fail on some subjects, and it holds across three tools (firmware fuzzing, ADS simulation, FuzzBench). The counter-instances give the condition under which generic guidance is enough: the subject shares the pattern.
- **Papers (3; testing):** dyma_fuzz_dynamic_direct_memory_acce, misbehavior_forecasting_for_focused, on_interaction_effects_in_greybox_fu
- **Instances (12):** testing/dyma_fuzz_dynamic_direct_memory_acce:66, testing/dyma_fuzz_dynamic_direct_memory_acce:127, testing/misbehavior_forecasting_for_focused:68, testing/misbehavior_forecasting_for_focused:69, testing/misbehavior_forecasting_for_focused:70, testing/misbehavior_forecasting_for_focused:71, testing/on_interaction_effects_in_greybox_fu:152, testing/on_interaction_effects_in_greybox_fu:155, testing/on_interaction_effects_in_greybox_fu:158, testing/on_interaction_effects_in_greybox_fu:159, testing/on_interaction_effects_in_greybox_fu:160, testing/on_interaction_effects_in_greybox_fu:165
- **Counter-instances (4):** testing/on_interaction_effects_in_greybox_fu:154, testing/on_interaction_effects_in_greybox_fu:164, testing/on_interaction_effects_in_greybox_fu:166, testing/on_interaction_effects_in_greybox_fu:163
- **Stem:** dynamic-testing | proposed | effectiveness

```
improve(effectiveness(failures, approach=dynamic-testing, guidance=subject-derived))
  because: assumptions(heuristic, fixed) = violated-by-some-subjects → targets(heuristic) = missed → effectiveness(failures) increasing-in subject-derived-guidance
  because: determinism(guidance, generic) = higher → exploration(tester) = lower → coverage(fuzzing) ≤ subject-derived
  unless: pattern(subject) = shared-with-generic | cost(derive-guidance) > gain
```

### NH-5

When an analysis pipeline has several input-preparation stages (feature-model extraction then CNF transformation; ordering of candidate methods in an LLM prompt), no single preparation choice is preferable in general, because each choice changes what the downstream engine sees (formula hardness, positional bias) and interacts with the other stages and with the use case, unless a task-derived signal is available (suspiciousness-based ordering), with randomisation as the fallback when it is not.

- **When:** multi-stage input preparation feeding a solver or LLM
- **Techniques:** none (phenomenon: input preparation choices), Kconfig feature-model extraction and CNF transformation, method ordering in LLM fault-localization prompts
- **Effect:** preferable choice depends on use case and on other pipeline stages
- **Because** (reader): encoding and ordering alter the downstream problem (tristate-as-bool lowers formula hardness; list position biases LLM attention), so their effects are conditional on other stages and on the analysis goal
- **Unless:** a task-derived ordering signal exists (DepGraph/Ochiai suspiciousness), which then dominates; cheap structural orders (CallGraph, LOC) for resource-limited settings
- **Stated:** implied · **Interesting:** yes — Condition changes which choice is best (interaction between stages); cross-area (formal, ai4se); denies the default belief that preprocessing is a neutral, one-best step.
- **Papers (2; ai4se, formal):** can_sat_solvers_keep_up_with_the_lin, order_matters_an_empirical_study_on
- **Instances (5):** formal/can_sat_solvers_keep_up_with_the_lin:242, formal/can_sat_solvers_keep_up_with_the_lin:244, ai4se/order_matters_an_empirical_study_on:158, ai4se/order_matters_an_empirical_study_on:159, ai4se/order_matters_an_empirical_study_on:161
- **Stem:** none | none | design

```
choose(preparation(input, approach=none), depends-on {use-case, other-stages})
  because: preparation(input) → shape(problem, downstream) → effect(preparation) moderated-by other-stages
  unless: signal(task-derived) = available → ordering = signal-based
  unless: signal(task-derived) = unavailable → ordering = random
```

### LB-B-2

When inputs carry more structural complexity (unbounded loops, grammatically complex perturbations), learning-based tools must place their reasoning deeper or make it more structured (inductive few-shot steps, deeper edit layers), because shallow or path-by-path processing cannot capture the semantics, so the right locus of intervention depends on the input and the model.

- **When:** inputs whose semantics exceed surface or single-pass processing (loops with infinitely many paths; grammatical perturbations of prompts)
- **Techniques:** llm-as-judge, model-editing
- **Effect:** the effective design point (k-induction reasoning; key edit layer) shifts deeper with input complexity
- **Because** (paper): loops have infinitely many paths, so few-shot examples of iterations are relevant by construction and correctness reasoning becomes k-induction (hoareprompt:53); higher grammatical complexity of a perturbation requires deeper semantic processing, so the key layer depends on perturbation type (creme:126)
- **Unless:** none stated; key layer also varies by model (creme:117), so no fixed depth transfers
- **Stated:** reader · **Interesting:** yes — cross-area mechanism linking program-verification prompting and model editing: structural complexity of the input dictates where the learned reasoning has to act
- **Papers (2; ai4se, formal):** creme_robustness_enhancement_of_code, hoareprompt_structural_reasoning_abo
- **Instances (3):** formal/hoareprompt_structural_reasoning_abo:53, ai4se/creme_robustness_enhancement_of_code:126, ai4se/creme_robustness_enhancement_of_code:117
- **Stem:** learning-based | proposed | design

```
depends-on(locus(reasoning, approach=learning-based), complexity(input, structural))
  because: loops(program) = infinite-paths → examples(few-shot, iterations) = relevant-by-construction → technique(correctness) = k-induction
  because: complexity(perturbation, grammatical) = higher → processing(semantic) = deeper → layer(key-layer) depends-on perturbation-type
  unless: layer(key-layer) depends-on model
```

### LB-A-4

When an LLM classifier, judge or translator is made to reason over an explicit, selective intermediate artefact (a structured natural-language requirement, natural-language postconditions at key program points, a generated specification), its quality rises over end-to-end prompting, most on hard inputs, because the LLM reasons more reliably in natural language over small pieces than directly in the target formalism or over a raw trajectory, unless the intermediate is exhaustive (full-state annotations, deeper unrolling) or wrong.

- **When:** the LLM's output is conditioned on a generated, structured intermediate (NL IR, NL strongest postconditions, specification) rather than on raw input alone
- **Techniques:** spec-inference, llm-as-judge
- **Effect:** raises accuracy, MCC, TPR and precision and lowers FPR of the classification or translation
- **Because** (paper): LLMs reason better in natural language than in temporal logic, so a structured NL IR yields fewer candidate errors; annotating only key transitions preserves the model's focus; a trajectory-only judgment is biased positive, so without a specification false positives stay high.
- **Unless:** the intermediate is exhaustive (full-state annotations gain +19.7% vs +62% for sparse; MCC peaks at k=3 unrolling), the generated specification is wrong, or the requirement text is ambiguous; FPR can still rise against the most lenient baseline
- **Stated:** implied · **Interesting:** yes — A cross-paper, cross-area mechanism (reason in NL over small pieces), with a condition that limits it: selective intermediates help, exhaustive ones dilute focus. Each paper argues for its own intermediate; none generalises.
- **Papers (3; ai4se, formal):** abstain_and_validate_a_dual_llm_poli, automating_requirements_formalizatio, hoareprompt_structural_reasoning_abo
- **Instances (23):** formal/automating_requirements_formalizatio:37, formal/automating_requirements_formalizatio:42, formal/automating_requirements_formalizatio:47, formal/automating_requirements_formalizatio:53, formal/automating_requirements_formalizatio:60, formal/automating_requirements_formalizatio:67, formal/automating_requirements_formalizatio:68, formal/automating_requirements_formalizatio:72, formal/hoareprompt_structural_reasoning_abo:72, formal/hoareprompt_structural_reasoning_abo:76, formal/hoareprompt_structural_reasoning_abo:77, formal/hoareprompt_structural_reasoning_abo:86, formal/hoareprompt_structural_reasoning_abo:90, formal/hoareprompt_structural_reasoning_abo:94, formal/hoareprompt_structural_reasoning_abo:107, formal/hoareprompt_structural_reasoning_abo:116, ai4se/abstain_and_validate_a_dual_llm_poli:69, ai4se/abstain_and_validate_a_dual_llm_poli:73, ai4se/abstain_and_validate_a_dual_llm_poli:76, ai4se/abstain_and_validate_a_dual_llm_poli:79, ai4se/abstain_and_validate_a_dual_llm_poli:81, ai4se/abstain_and_validate_a_dual_llm_poli:82, ai4se/abstain_and_validate_a_dual_llm_poli:83
- **Counter-instances (6):** formal/hoareprompt_structural_reasoning_abo:118, formal/hoareprompt_structural_reasoning_abo:127, formal/hoareprompt_structural_reasoning_abo:96, formal/hoareprompt_structural_reasoning_abo:97, formal/automating_requirements_formalizatio:51, formal/automating_requirements_formalizatio:65
- **Stem:** learning-based | proposed | quality

```
improve(quality(classification, approach=learning-based, intermediate=structured-nl))
  because: reasoning(llm, nl) > reasoning(llm, formal) → errors(candidates, structured-nl-ir) < errors(candidates, direct)
  because: annotations(program-points) = key-transitions-only → focus(llm) = preserved
  unless: annotations(program-points) = full-state | unrolling(k) > 3 | specification(generated) = wrong
```

### DT-10

When a subject's language or input format is permissive, coarse perturbations stay valid, so removed-call mutants survive tests and chunk mutations are learned as productive. Strict formats break coarse perturbations and push toward fine-grained unit mutations, because whether the perturbed artefact is still valid decides whether it runs meaningfully. This does not apply to patterns unrelated to validity, such as libpcap's stacked arithmetic mutators.

- **When:** permissive semantics or input formats (Python truthiness; lenient file formats such as freetype's)
- **Techniques:** mutation-testing, greybox-fuzzing
- **Effect:** more coarse-perturbation survivors and higher learned probability of chunk mutators; under strict formats (openssl, libpng) unit mutators are more likely
- **Because** (paper): truthiness(python, object) = permissive → expression(call-removed) = valid → survivors > 0; format(input, openssl) = strict → inputs(chunk-mutated) = broken → probability(unit) > chunk
- **Unless:** the learned regularity does not depend on validity (libpcap: arithmetic mutators stack and are followed by clone mutators)
- **Stated:** reader · **Interesting:** yes — Both mechanisms come from the papers, and strictness flips the preferred mutation grain (chunk vs unit). It links mutation-testing survivors and fuzzing mutator schedules through one property of the subject.
- **Papers (2; testing):** hybrid_fault_driven_mutation_testing, on_interaction_effects_in_greybox_fu
- **Instances (3):** testing/hybrid_fault_driven_mutation_testing:58, testing/on_interaction_effects_in_greybox_fu:174, testing/on_interaction_effects_in_greybox_fu:176
- **Counter-instances (1):** testing/on_interaction_effects_in_greybox_fu:178
- **Stem:** dynamic-testing | proposed | prevalence

```
more(prevalence(coarse-perturbation-success, approach=dynamic-testing, format=permissive))
  because: truthiness(python, object) = permissive → expression(call-removed) = valid → survivors(tests) > 0
  because: format(input) = strict → inputs(chunk-mutated) = broken → probability(next-mutator, unit) > chunk
  unless: regularity(learned) independent-of validity
```


## T8 Difficulty scales with size, validation does not

### NH-4

When the material to analyse grows (Linux feature models year over year, integration-test logs across many files, proof-search trees with more models, repeated test runs for dynamic analysis), the cost of analysis grows with it, whether the analyser is a SAT solver or a developer, because cost tracks the volume and noise of what must be processed, unless per-unit processing speeds up faster than the material grows, as algorithmic SAT advances do on a fixed benchmark.

- **When:** analysis of a growing or noisy object by a fixed-capability analyser (solver, search, human diagnoser)
- **Techniques:** none (phenomenon: analysis cost vs object size), SAT solving on feature models, manual diagnosis of integration-test failures, multi-model proof search, dynamic analysis for runtime verification
- **Effect:** time/overhead/nodes increase with object size or year
- **Because** (paper): count(features, linux) increasing-in year -> runtime(sat, linux) increasing-in year; generic driver errors -> many log files -> benign-error noise -> cognitive load -> diagnosis time > unit-test diagnosis; runs(tests) = 2 -> overhead >= 2x
- **Unless:** speedup(sat, algorithms) > growth(complexity, linux); on fixed benchmarks newer solvers are faster, and not every solver innovation helps on a given domain
- **Stated:** implied · **Interesting:** yes — Paper-stated mechanisms and an explicit unless-condition that flips the direction (algorithmic speedup on fixed inputs); the same cost law covers machine solvers and human diagnosers across formal and ai4se.
- **Papers (4; ai4se, formal):** can_sat_solvers_keep_up_with_the_lin, fine_grained_analyses_for_evolution_, llm_based_automated_diagnosis_of_int, proofcoop_collaborative_automated_fo
- **Instances (11):** formal/can_sat_solvers_keep_up_with_the_lin:48, formal/can_sat_solvers_keep_up_with_the_lin:51, formal/can_sat_solvers_keep_up_with_the_lin:63, formal/can_sat_solvers_keep_up_with_the_lin:78, formal/can_sat_solvers_keep_up_with_the_lin:105, formal/can_sat_solvers_keep_up_with_the_lin:155, formal/can_sat_solvers_keep_up_with_the_lin:166, ai4se/llm_based_automated_diagnosis_of_int:39, ai4se/llm_based_automated_diagnosis_of_int:41, formal/proofcoop_collaborative_automated_fo:137, formal/fine_grained_analyses_for_evolution_:125
- **Counter-instances (3):** formal/can_sat_solvers_keep_up_with_the_lin:211, formal/can_sat_solvers_keep_up_with_the_lin:64, formal/can_sat_solvers_keep_up_with_the_lin:71
- **Stem:** none | none | cost

```
increase(cost(analysis, approach=none), size(object))
  because: size(object) increasing-in time → work(analysis) increasing-in size(object) → runtime(analysis) increasing-in time
  because: files(logs, test=integration) = many → noise(logs) = high → load(cognitive, diagnosis) = high → time(diagnosis) > ?
  unless: speedup(analyser, algorithms) > growth(complexity, object)
```

### NH-7

When the objects SE techniques target keep growing or are drawn from the hard tail (Linux feature models growing by hundreds of features a year, C programs beyond 100 LoC, theorems whose human proofs exceed 20 tactics, highly obfuscated JavaScript many times larger), what remains unsolved is disproportionately the large objects, because difficulty scales with size while techniques are validated on small ones, unless the technique's cost is bounded by a structural quantity independent of object size.

- **When:** techniques applied to objects whose size grows over time or across difficulty levels
- **Techniques:** none (phenomenon: object size), SAT-based feature-model analysis, C-to-Rust translation, LLM proof synthesis in Coq, JavaScript deobfuscation datasets
- **Effect:** object size grows (linearly in time, steeply in difficulty) and prior techniques cover only the small end
- **Because** (reader): work and failure probability scale with size (features, LoC, tactics), so benchmarks of small objects overstate coverage of real, growing systems
- **Unless:** summaries whose time and space are O(T) in the number of values in an interval, independent of program size, or decomposition of the object
- **Stated:** implied · **Interesting:** yes — Cross-area pattern (formal, ai4se) with a condition (size-independent structural bounds escape it); denies the default that benchmark success transfers to growing real systems.
- **Papers (4; ai4se, formal):** can_sat_solvers_keep_up_with_the_lin, cascade_llm_powered_javascript_deobf, cobblestone_a_divide_and_conquer_app, evoc2rust_a_skeleton_guided_framewor
- **Instances (8):** formal/can_sat_solvers_keep_up_with_the_lin:198, formal/can_sat_solvers_keep_up_with_the_lin:201, formal/can_sat_solvers_keep_up_with_the_lin:204, formal/can_sat_solvers_keep_up_with_the_lin:206, formal/can_sat_solvers_keep_up_with_the_lin:219, ai4se/evoc2rust_a_skeleton_guided_framewor:228, formal/cobblestone_a_divide_and_conquer_app:240, ai4se/cascade_llm_powered_javascript_deobf:153
- **Counter-instances (2):** formal/loopscc_summarizing_complex_multi_br:66, formal/loopscc_summarizing_complex_multi_br:71
- **Stem:** none | none | scale

```
decrease(coverage(technique, approach=none), size(object))
  because: size(object) increasing-in time | difficulty → work(technique) increasing-in size(object) → unsolved(objects) concentrated-in size=large
  unless: complexity(technique) ∝ values-in-interval | decomposition = on
```

### LB-A-13

When LLM-based code generation or translation is applied to larger or more complex projects, absolute success (compile, test pass, coverage) falls, because intricate control and data dependencies strain the model's comprehension, while the margin over weaker LLM baselines widens and size-normalised rates such as the safe-code rate can rise, because a fixed set of encapsulated unsafe mappings is diluted in larger code.

- **When:** the same LLM-based tool is applied across projects of different size or share of complex methods
- **Techniques:** test-generation, translation
- **Effect:** absolute effectiveness decreases in complexity/LOC; relative margin over baselines and safe-rate increase
- **Because** (paper): Intricate control flow and data dependencies challenge LLM comprehension, so test rate falls with complexity; unsafe code is confined to a fixed set of mappings, so its share falls as LOC grows.
- **Unless:** the complexity measure misses what matters (CITYWALK's leveldb has only 2.7% complex methods yet 47.83% compile success)
- **Stated:** yes · **Interesting:** yes — Complexity changes the sign depending on the measure: absolute success falls, relative margin and size-normalised safety rise. Mechanisms are paper-stated, and the law holds across testing and ai4se.
- **Papers (2; ai4se, testing):** citywalk_enhancing_llm_based_c_unit, evoc2rust_a_skeleton_guided_framewor
- **Instances (5):** testing/citywalk_enhancing_llm_based_c_unit:157, testing/citywalk_enhancing_llm_based_c_unit:161, ai4se/evoc2rust_a_skeleton_guided_framewor:146, ai4se/evoc2rust_a_skeleton_guided_framewor:186, ai4se/evoc2rust_a_skeleton_guided_framewor:80
- **Counter-instances (1):** ai4se/evoc2rust_a_skeleton_guided_framewor:191
- **Stem:** learning-based | proposed | effectiveness

```
worsen(effectiveness(code-generation, approach=learning-based, complexity=high))
  because: control-flow(project, data-dependencies) = intricate → comprehension(llm, code) = challenged → test-rate decreasing-in complexity
  because: unsafe-code(mappings, encapsulated) = fixed-set → share(unsafe-code, project) decreasing-in loc → safe-rate increasing-in loc
  unless: share(methods, complex) = low ∧ csr(unit-test) = low | leveldb
```

### LB-A-2

When a learning-based tool exposes a depth or breadth knob (LLM refinement iterations, unrolling depth k, trace-grouping parameter d, number of models whose tactics are pooled), its machine cost grows with that knob rather than with the size of the artefact, because each increment adds LLM rounds or search branches, unless lowering the knob is paid for elsewhere, as more human validation queries or lower classification quality.

- **When:** the tool's cost is driven by an iteration, depth or pooling parameter
- **Techniques:** translation, llm-as-judge, spec-inference, proof-synthesis
- **Effect:** machine cost (seconds per LOC, tokens, trace-generation latency, search steps) increases in the knob and is independent of code size
- **Because** (paper): Project-specific complexity sets how many LLM refinement iterations are needed, so seconds per LOC depend on iterations, not LOC; a larger BDTG group size d searches more trace combinations, so latency rises with d.
- **Unless:** a lower d means more traces for the user to validate (ARTEMIS); in HoarePrompt, quality peaks at k=3 while tokens keep rising with k, so the knob is not a free quality dial
- **Stated:** implied · **Interesting:** yes — Denies the default belief that LLM tool cost scales with input size (EvoC2Rust: seconds per LOC uncorrelated with LOC), gives a paper-stated mechanism (refinement iterations), and names the condition (the knob trades machine time against human effort or quality).
- **Papers (4; ai4se, formal):** automating_requirements_formalizatio, evoc2rust_a_skeleton_guided_framewor, hoareprompt_structural_reasoning_abo, proofcoop_collaborative_automated_fo
- **Instances (6):** ai4se/evoc2rust_a_skeleton_guided_framewor:197, ai4se/evoc2rust_a_skeleton_guided_framewor:198, formal/automating_requirements_formalizatio:134, formal/automating_requirements_formalizatio:138, formal/hoareprompt_structural_reasoning_abo:134, formal/proofcoop_collaborative_automated_fo:128
- **Stem:** learning-based | proposed | cost

```
increase(cost(latency, approach=learning-based, depth-knob=raised))
  because: iterations(llm-refinement) depends-on complexity(code, project) → sec-per-loc depends-on iterations → sec-per-loc independent-of loc
  because: groupsize(bdtg, d) increasing → latency(trace-generation) increasing-in d
  unless: effort(validation, traces) increasing-in lower-d
```

### AV-3

When verifying code units or generating verification harnesses, effort and failure grow with how much of the environment must be modeled (variable and function models, user-defined input types), not with raw code size, because each unknown input must be modeled or nondeterministically constructed, unless the property needs only generic models (return-by-type for memory safety) or a dependency graph supplies the constructors.

- **When:** bounded model checking of units or harnesses that need models of unknown variables, functions or input types
- **Techniques:** bounded-model-checking
- **Effect:** developer time and generation errors increase with number of environment models; execution time barely tracks program size
- **Because** (paper): unit size -> more variable models -> modeling time; user-defined parameter types -> nondet-construction errors -> lower first-round success (paper because lines)
- **Unless:** memory-safety verification mostly needs return-value-by-type models (86/113), so modeling effort is avoided; full dependency-graph support for user-defined types restores harness success
- **Stated:** implied · **Interesting:** yes — denies the default that verification effort tracks LoC (r2 of exec-time on program size = 0.029; proof size vs function size only r2 = 0.328); a shared mechanism across proof writing and LLM harness generation
- **Papers (2; formal):** do_unit_proofs_work_an_empirical_stu, harnessllm_rust_verification_harness
- **Instances (8):** formal/do_unit_proofs_work_an_empirical_stu:125, formal/do_unit_proofs_work_an_empirical_stu:142, formal/do_unit_proofs_work_an_empirical_stu:143, formal/do_unit_proofs_work_an_empirical_stu:196, formal/do_unit_proofs_work_an_empirical_stu:110, formal/harnessllm_rust_verification_harness:87, formal/harnessllm_rust_verification_harness:91, formal/harnessllm_rust_verification_harness:111
- **Counter-instances (1):** formal/do_unit_proofs_work_an_empirical_stu:103
- **Stem:** analysis-and-verification | proposed | cost

```
worsen(cost(verification, approach=analysis-and-verification, environment-models=many))
  because: count(models, variables|types) increasing-in unknown-inputs → errors(construction)|dev-time(modeling) increasing-in models → cost(verification) increasing-in models
  unless: cost(verification) = low | property=memory-safety-return-models, dependency-graph=available
```

### DT-5

When a guided dynamic tester is compared with its baseline, its advantage is much larger in time to reach a result (hours earlier to the baseline's final coverage, several times more collisions per hour, minutes rather than hours to a bug) than in final totals. The likely reason is that each new branch or failure costs more than the last, so a small gain in the final total means a large time saving. This does not hold when the guided tool's final total does not beat the baseline's: then it never reaches the baseline's plateau.

- **When:** guided tester's final total ≥ baseline's final total on the subject
- **Techniques:** greybox-fuzzing, simulation-testing
- **Effect:** time-to-coverage, time-to-bug and failures-per-hour improve by far more (hours; 1.4-4×) than final coverage or failure totals (often +0.1% to +6%)
- **Because** (reader): cost(new-branch) grows exponentially with campaign length (cited by on_interaction from Böhme and Falk), so the last few percent of coverage take most of the time
- **Unless:** the guided tool's final total is ≤ the baseline's (php and openssl vs AFL++/MOPT, freetype vs MOPT); the comparison uses only bugs every tool finds (time-to-bug ns); per-execution instrumentation slows the tool (≈4× vs oracle)
- **Stated:** implied · **Interesting:** yes — Denies the belief that a +0.1-6% coverage gain is negligible: the same gain means reaching the plateau hours earlier. Foresee finds fewer collisions in total than exhaustive search but 3-4× more per hour. The counter-instances give the condition under which the time advantage turns into a deficit.
- **Papers (3; testing):** dyma_fuzz_dynamic_direct_memory_acce, misbehavior_forecasting_for_focused, on_interaction_effects_in_greybox_fu
- **Instances (15):** testing/on_interaction_effects_in_greybox_fu:114, testing/on_interaction_effects_in_greybox_fu:115, testing/on_interaction_effects_in_greybox_fu:116, testing/on_interaction_effects_in_greybox_fu:117, testing/on_interaction_effects_in_greybox_fu:119, testing/on_interaction_effects_in_greybox_fu:120, testing/on_interaction_effects_in_greybox_fu:121, testing/misbehavior_forecasting_for_focused:86, testing/misbehavior_forecasting_for_focused:87, testing/misbehavior_forecasting_for_focused:97, testing/misbehavior_forecasting_for_focused:98, testing/dyma_fuzz_dynamic_direct_memory_acce:169, testing/dyma_fuzz_dynamic_direct_memory_acce:170, testing/dyma_fuzz_dynamic_direct_memory_acce:171, testing/dyma_fuzz_dynamic_direct_memory_acce:172
- **Counter-instances (5):** testing/on_interaction_effects_in_greybox_fu:118, testing/on_interaction_effects_in_greybox_fu:122, testing/on_interaction_effects_in_greybox_fu:123, testing/on_interaction_effects_in_greybox_fu:141, testing/dyma_fuzz_dynamic_direct_memory_acce:186
- **Stem:** dynamic-testing | proposed | cost

```
reduce(cost(time-to-result, approach=dynamic-testing, guidance=on))
  because: cost(new-branch) increasing-in campaign-time → time(last-few-percent) = dominant → time-to-coverage(baseline-final) ≪ campaign-length when total(guided) > total(baseline)
  unless: total(guided) ≤ total(baseline) | bugs = common-to-all | overhead(instrumentation, per-execution) = high
```


## T9 Combining members gives diminishing returns

### LB-B-9

When proof-synthesis effectiveness is raised by adding or swapping LLMs, gains depend on which model is used and saturate as models are combined, because the models share a ceiling on usable tactics so each added model contributes less that the others have not already found.

- **When:** effectiveness is increased by model choice or by ensembling more models
- **Techniques:** proof-synthesis
- **Effect:** theorems proved saturating-in number of models; success rate depends on the LLM (not ordered by presumed model strength)
- **Because** (paper): ability of models to find usable tactics has a ceiling, so the gain from each added model decreases (proofcoop:151)
- **Unless:** none stated; Claude 3 outperforming GPT-4 in ChainOfThought (cobblestone:120) shows the ordering is not a simple function of model generation
- **Stated:** implied · **Interesting:** yes — mechanism (shared tactic ceiling) that denies the default that more or bigger models add proportionally
- **Papers (2; formal):** cobblestone_a_divide_and_conquer_app, proofcoop_collaborative_automated_fo
- **Instances (6):** formal/proofcoop_collaborative_automated_fo:151, formal/proofcoop_collaborative_automated_fo:192, formal/proofcoop_collaborative_automated_fo:195, formal/cobblestone_a_divide_and_conquer_app:118, formal/cobblestone_a_divide_and_conquer_app:119, formal/cobblestone_a_divide_and_conquer_app:120
- **Stem:** learning-based | baseline | effectiveness

```
saturate(effectiveness(proofs, approach=learning-based), models)
  because: ability(models, usable-tactics) = ceiling → gain(added-model) decreasing-in models → theorems(proved) saturating-in models
  because: success-rate(theorem, coq) depends-on llm
```

### LB-A-11

When several learned provers, prompt variants or LLM filters are combined, union-style combination that keeps every member's candidates raises success, most when members are individually weak and err differently, because diversity covers goals or bugs no single member reaches, unless members are combined by trusting their confidence (voting, bidding, stacking), where confident but poor members crowd out good tactics and the gain shrinks as members are added.

- **When:** multiple learned components (models, prompt variants, filtering policies) contribute candidates to one result
- **Techniques:** proof-synthesis, llm-as-judge
- **Effect:** raises theorems proved and fail-to-pass rate for union/complementary combinations; selection-by-confidence combinations gain little and lose gain with more members
- **Because** (paper): Varying prompts and context increases proof diversity, which raises success; abstention judges report quality independent of the patch, so it complements a persuadable patch validator; in preferential prediction, poor models predict with high certainty, so more poor tactics are tried as poor members are added.
- **Unless:** combination by confidence (ProofCoop voting, bidding, stacking, sharing: improvement decreasing in number of models; stacking below Diva); restricting preferential prediction to the top-3 models partly recovers it
- **Stated:** implied · **Interesting:** yes — Resolves ProofCoop's better/worse split with a condition (union vs confidence-based selection) and adds a second (weak, diverse members benefit most); Cobblestone and Abstain-and-Validate show the same union/complementarity effect in other settings. Denies the default belief that adding more models or trusting the most confident one helps.
- **Papers (3; ai4se, formal):** abstain_and_validate_a_dual_llm_poli, cobblestone_a_divide_and_conquer_app, proofcoop_collaborative_automated_fo
- **Instances (16):** formal/proofcoop_collaborative_automated_fo:81, formal/proofcoop_collaborative_automated_fo:86, formal/proofcoop_collaborative_automated_fo:92, formal/proofcoop_collaborative_automated_fo:94, formal/proofcoop_collaborative_automated_fo:157, formal/proofcoop_collaborative_automated_fo:158, formal/proofcoop_collaborative_automated_fo:159, formal/proofcoop_collaborative_automated_fo:162, formal/proofcoop_collaborative_automated_fo:187, formal/cobblestone_a_divide_and_conquer_app:65, formal/cobblestone_a_divide_and_conquer_app:208, formal/cobblestone_a_divide_and_conquer_app:210, ai4se/abstain_and_validate_a_dual_llm_poli:127, ai4se/abstain_and_validate_a_dual_llm_poli:130, ai4se/abstain_and_validate_a_dual_llm_poli:133, ai4se/abstain_and_validate_a_dual_llm_poli:140
- **Counter-instances (7):** formal/proofcoop_collaborative_automated_fo:153, formal/proofcoop_collaborative_automated_fo:154, formal/proofcoop_collaborative_automated_fo:155, formal/proofcoop_collaborative_automated_fo:156, formal/proofcoop_collaborative_automated_fo:104, formal/proofcoop_collaborative_automated_fo:109, formal/proofcoop_collaborative_automated_fo:171
- **Stem:** learning-based | proposed | effectiveness

```
improve(effectiveness(proofs, approach=learning-based, combination=union))
  because: variation(prompt, context) increasing → diversity(candidates) increasing → success-rate increasing-in diversity
  because: improvement(combination) decreasing-in component-accuracy
  unless: combination = confidence-selection → certainty(prediction, bad-model) = high → tactics(poor, tried) increasing-in bad-models → theorems decreasing-in bad-models
```

### LB-A-6

When a learning-based scaffold (structural prompting, NL intermediate, probing-based ranking) is added to an LLM, its relative quality gain is largest for smaller or weaker models and harder inputs, because the scaffold supplies structure the model cannot supply itself, unless the task is so hard that the model carries little usable signal, where execution feedback or the model's own calibration beats the scaffold; part of the widening is a ratio effect of low baselines.

- **When:** the same scaffold is compared across base models of differing strength or across easy and hard input subsets
- **Techniques:** llm-as-judge, spec-inference, probing
- **Effect:** relative gain over baselines decreases with model size/strength and increases with input difficulty, reversing at the hardest tasks
- **Because** (reader): A scaffold adds reasoning structure whose marginal value is highest where the model's unaided reasoning is weakest; when the task exceeds the model, execution feedback carries more information than the model's internal representation.
- **Unless:** task difficulty is high enough that execution feedback is worth more (RankEF beats LAT at rank 1 for CodeLlama on BigCodeBench) or the model is well calibrated on its own candidates (Intrinsic beats LAT for CodeLlama on HumanEval); on some hard subsets the gain vanishes (parity)
- **Stated:** implied · **Interesting:** yes — Gives a condition that reverses the direction: gains grow as the base gets weaker or the input harder, until the model has too little signal, at which point feedback-based baselines win. The glosses warn that some of the growth is a ratio effect, which keeps the law honest.
- **Papers (3; ai4se, formal):** automating_requirements_formalizatio, hoareprompt_structural_reasoning_abo, on_llms_internal_representation_of_c
- **Instances (9):** formal/hoareprompt_structural_reasoning_abo:87, formal/hoareprompt_structural_reasoning_abo:80, formal/automating_requirements_formalizatio:55, formal/automating_requirements_formalizatio:56, formal/automating_requirements_formalizatio:57, formal/automating_requirements_formalizatio:61, formal/automating_requirements_formalizatio:66, formal/automating_requirements_formalizatio:67, ai4se/on_llms_internal_representation_of_c:111
- **Counter-instances (3):** ai4se/on_llms_internal_representation_of_c:132, ai4se/on_llms_internal_representation_of_c:114, formal/automating_requirements_formalizatio:65
- **Stem:** learning-based | proposed | quality

```
improve(quality(classification, approach=learning-based, scaffold=on, base-strength=low))
  because: reasoning(llm, unaided) decreasing-in difficulty → value(scaffold) increasing-in difficulty
  unless: difficulty(task, benchmark) = high → value(execution-feedback, ranking) increasing-in difficulty → accuracy(ranking, k=1) decreasing-in difficulty
```


## T10 Adoption depends on trust, not accuracy alone

### NH-1

When practitioners gain experience with a specific technology or practice (mobile accessibility guidelines, LLM assistants), the breadth of what they do and value grows (more features, more testing techniques, more weight on interpretability and workflow integration) while their attention to safeguards (controllability, ethical compliance) shrinks, because familiarity raises trust and trust lowers concern about the tool's benevolence, unless the experience is general SE experience rather than experience with that technology, or the practitioner sits at an intermediate level where emphasis dips non-monotonically.

- **When:** practitioner populations stratified by experience or familiarity with a specific technology (accessibility guidelines on mobile, LLMs for SE)
- **Techniques:** none (phenomenon: practitioner experience), mobile accessibility practice, LLM4SE trust antecedents
- **Effect:** breadth of practice and valued integration/interpretability increase with experience; emphasis on safeguard antecedents (controllability, ethicality) decreases
- **Because** (paper): familiarity(tool) increasing-in experience -> trust(tool) increasing-in experience -> concern(usage, benevolence) decreasing-in experience -> selection(controllability, ethicality) decreasing-in experience (paper, mapping_the_trust_terrain:125, assuming calibrated trust); familiarity with guidelines enables more features and techniques (reader)
- **Unless:** experience measured as general SE knowledge (no relationship with community-factor selection) or intermediate experience (robustness emphasis dips below both novices and experts)
- **Stated:** implied · **Interesting:** yes — Resolves more/less contraries with one condition (breadth vs safeguards), carries a paper-stated causal chain, and spans human and ai4se; denies the default belief that experience uniformly improves diligence.
- **Papers (2; ai4se, human):** mapping_the_trust_terrain_llms_in_so, practitioner_views_on_mobile_app_access
- **Instances (9):** human/practitioner_views_on_mobile_app_access:57, human/practitioner_views_on_mobile_app_access:82, human/practitioner_views_on_mobile_app_access:110, human/practitioner_views_on_mobile_app_access:111, ai4se/mapping_the_trust_terrain_llms_in_so:121, ai4se/mapping_the_trust_terrain_llms_in_so:125, ai4se/mapping_the_trust_terrain_llms_in_so:130, ai4se/mapping_the_trust_terrain_llms_in_so:131, ai4se/mapping_the_trust_terrain_llms_in_so:133
- **Counter-instances (2):** ai4se/mapping_the_trust_terrain_llms_in_so:124, ai4se/mapping_the_trust_terrain_llms_in_so:134
- **Stem:** none | none | prevalence

```
increase(breadth(practice, approach=none, pop=practitioners), experience=high)
  because: familiarity(tool) increasing-in experience → trust(tool) increasing-in experience → concern(safeguards) decreasing-in experience → selection(antecedent=controllability|ethicality) decreasing-in experience
  because: familiarity(guidelines) increasing-in experience → features(implemented) increasing-in experience
  unless: experience = general-se | experience = intermediate
  assume: trust(tool) = calibrated
```

### NH-6

When evaluating tools whose value depends on human adoption (reviewer recommenders, LLM assistants for SE), a single accuracy metric is insufficient and evaluation needs a multi-item set of goal and guardrail measures (time in review, time spent, latency, clicks, workload; security, understandability, trust over time and per task), because a single metric can look good while humans rubber-stamp or over-trust the output.

- **When:** evaluation design for tools embedded in human workflows
- **Techniques:** none (phenomenon: evaluation instruments), code reviewer recommendation, LLM4SE trustworthiness measurement
- **Effect:** recommended evaluation instrument is multi-metric, task-specific and longitudinal
- **Because** (reader): human behaviour mediates tool value; accuracy-only metrics miss rubber-stamping (TimeSpent guardrail) and trust that builds or erodes over time
- **Unless:** none stated
- **Stated:** yes · **Interesting:** yes — Cross-area pattern (human, ai4se) with a mechanism: both communities independently reject single-metric evaluation for human-adopted tools for the same reason.
- **Papers (2; ai4se, human):** improving_code_reviewer_recommendation, mapping_the_trust_terrain_llms_in_so
- **Instances (5):** human/improving_code_reviewer_recommendation:135, ai4se/mapping_the_trust_terrain_llms_in_so:201, ai4se/mapping_the_trust_terrain_llms_in_so:202, ai4se/mapping_the_trust_terrain_llms_in_so:203, ai4se/mapping_the_trust_terrain_llms_in_so:204
- **Stem:** none | none | design

```
require(metrics(evaluation, approach=none, tool=human-adopted) = multi-item)
  because: value(tool) moderated-by behaviour(human) → accuracy(tool) ↛ value(tool) → guardrail(time-spent, trust-over-time) needed
  unless: none
```

### NH-8

When automated SE tools deliver output inside developers' workflow (integration-test diagnoses, LLM code suggestions), developers' trust and perceived usefulness are governed first by consistent accuracy (occasional errors are tolerated, but frequent inaccuracies or unpredictable output erode trust), because unhelpful in-workflow findings cost attention and developers are sensitive to them, and perceived risk varies by task, unless users are novices whose trust is not calibrated to accuracy (they credit machine-written code's security or accept output without scrutiny).

- **When:** developers using automated diagnosis or LLM generation tools in their workflow
- **Techniques:** none (phenomenon: developer trust and perception), LLM-based failure diagnosis, LLM code generation, test generation, repair
- **Effect:** trust/perception increases with consistent correctness; distrust increases with inaccuracy and unpredictability; level depends on task risk
- **Because** (paper): findings(unhelpful, automatic) = in-workflow -> sensitivity(developer, unhelpful) = high -> perception depends-on accuracy; risk(task, perceived) depends-on task -> trust depends-on task
- **Unless:** novice users: trust in machine-generated code's security exceeds trust in human code, and scrutiny falls as trust rises; trust also depends on usability, workflow integration and community signals
- **Stated:** implied · **Interesting:** yes — Paper-stated mechanism plus a condition (novice, uncalibrated trust) under which the accuracy-trust link breaks; generalises from one diagnosis tool to LLM assistants at large.
- **Papers (2; ai4se):** llm_based_automated_diagnosis_of_int, mapping_the_trust_terrain_llms_in_so
- **Instances (6):** ai4se/llm_based_automated_diagnosis_of_int:130, ai4se/mapping_the_trust_terrain_llms_in_so:85, ai4se/mapping_the_trust_terrain_llms_in_so:92, ai4se/mapping_the_trust_terrain_llms_in_so:93, ai4se/mapping_the_trust_terrain_llms_in_so:151, ai4se/mapping_the_trust_terrain_llms_in_so:175
- **Counter-instances (3):** ai4se/mapping_the_trust_terrain_llms_in_so:147, ai4se/mapping_the_trust_terrain_llms_in_so:157, ai4se/mapping_the_trust_terrain_llms_in_so:170
- **Stem:** none | none | perception

```
increase(trust(tool, approach=none, pop=developers), consistency(correctness))
  because: findings(unhelpful) = in-workflow → sensitivity(developer, unhelpful) = high → perception(tool) depends-on accuracy
  because: risk(task, perceived) depends-on task → trust(tool) depends-on task
  unless: pop = novices → trust(tool) ≠ calibrated → scrutiny(output) decreasing-in trust
```

### LB-B-5

When usability is judged on surface form or engagement (test naming, layout, click-through, feedback rate), learning-based tools match or beat LLM baselines and even human work, but when it depends on expert domain judgement (assertion quality, choosing a competent reviewer) they fall short of humans, because the models optimise generic form or a proxy objective (workload) that users override when it conflicts with expertise.

- **When:** usability aspect is surface/presentation or engagement rather than expert semantic judgement
- **Techniques:** test-generation, learned-ranking, llm-prompting
- **Effect:** better/parity on surface usability; worse than human on expertise-bound usability
- **Because** (reader): generated artefacts inherit generic conventions of form, while expert-bound qualities need project/domain knowledge the model lacks; a ranking that puts a low-workload but inexpert candidate first is skipped by authors
- **Unless:** the aspect requires domain expertise (assertion oracles; reviewer competence) — then human work wins and users route around the tool
- **Stated:** reader · **Interesting:** yes — resolves better/worse instances with a condition (surface vs expertise-bound usability) across testing, human and ai4se studies
- **Papers (3; ai4se, human, testing):** citywalk_enhancing_llm_based_c_unit, improving_code_reviewer_recommendation, llm_based_automated_diagnosis_of_int
- **Instances (8):** testing/citywalk_enhancing_llm_based_c_unit:222, testing/citywalk_enhancing_llm_based_c_unit:223, testing/citywalk_enhancing_llm_based_c_unit:224, testing/citywalk_enhancing_llm_based_c_unit:225, testing/citywalk_enhancing_llm_based_c_unit:228, human/improving_code_reviewer_recommendation:53, ai4se/llm_based_automated_diagnosis_of_int:105, ai4se/llm_based_automated_diagnosis_of_int:111
- **Counter-instances (5):** testing/citywalk_enhancing_llm_based_c_unit:229, testing/citywalk_enhancing_llm_based_c_unit:231, human/improving_code_reviewer_recommendation:105, human/improving_code_reviewer_recommendation:106, human/improving_code_reviewer_recommendation:104
- **Stem:** learning-based | proposed | usability

```
improve(usability(outputs, approach=learning-based, aspect=surface))
  because: conventions(form, generic) = learned → naming(test-case), layout(test-case) ≥ human
  because: objective(ranking) = workload-proxy → top1(candidate) = inexpert → authors skip → clicks(top1) decreasing
  unless: aspect(usability) = expertise-bound
```

### AV-5

When users or LLMs must find and fix problems under a small budget or on harder tasks, analysis support (online diagnostics, chain-of-thought construction guidance, protected-region repair prompts) raises success because it removes construction errors and surfaces issues early, unless the task is simple or many repair rounds are allowed, where unaided variants catch up, and possibly for subtle real-world bugs the warnings do not target.

- **When:** debugging or harness generation with a limited attempt budget or tasks of increasing difficulty
- **Techniques:** interactive-debugging, bounded-model-checking
- **Effect:** success-rate and early detection increase with guidance; the gap shrinks on simple tasks and with more rounds
- **Because** (paper): guided incremental nondet construction lowers construction errors; online diagnostics make issues visible during inference so they are detected early (paper because lines)
- **Unless:** simplest task (Holmes task A: no significant difference); 10 repair rounds (ablation gaps fall to -1.0% and -0.4%, all LLMs reach 100%); subtle real-world bugs, for which the authors expect smaller effects
- **Stated:** implied · **Interesting:** yes — cross-area (human debugging and LLM harness generation); the condition (task difficulty and retry budget) changes the size and significance of the effect; mechanism from paper because lines
- **Papers (2; formal, testing):** harnessllm_rust_verification_harness, online_and_interactive_bayesian_infe
- **Instances (25):** testing/online_and_interactive_bayesian_infe:38, testing/online_and_interactive_bayesian_infe:40, testing/online_and_interactive_bayesian_infe:42, testing/online_and_interactive_bayesian_infe:44, testing/online_and_interactive_bayesian_infe:46, testing/online_and_interactive_bayesian_infe:48, testing/online_and_interactive_bayesian_infe:67, formal/harnessllm_rust_verification_harness:87, formal/harnessllm_rust_verification_harness:90, formal/harnessllm_rust_verification_harness:91, formal/harnessllm_rust_verification_harness:93, formal/harnessllm_rust_verification_harness:94, formal/harnessllm_rust_verification_harness:95, formal/harnessllm_rust_verification_harness:96, formal/harnessllm_rust_verification_harness:98, formal/harnessllm_rust_verification_harness:100, formal/harnessllm_rust_verification_harness:101, formal/harnessllm_rust_verification_harness:102, formal/harnessllm_rust_verification_harness:118, formal/harnessllm_rust_verification_harness:119, formal/harnessllm_rust_verification_harness:122, formal/harnessllm_rust_verification_harness:131, formal/harnessllm_rust_verification_harness:132, formal/harnessllm_rust_verification_harness:133, formal/harnessllm_rust_verification_harness:134
- **Counter-instances (1):** testing/online_and_interactive_bayesian_infe:32
- **Stem:** analysis-and-verification | proposed | effectiveness

```
improve(effectiveness(success-rate, approach=analysis-and-verification, guidance=on))
  because: guidance(construction|online-diagnostics) = on → errors(construction)|time-to-detection decreasing-in guidance → success-rate increasing-in guidance
  because: rounds(repair) increasing → success-rate(unguided) → success-rate(guided)
  unless: effect(guidance) = null | task=simple, rounds>=10, bugs=subtle-real-world
```


## Interesting laws outside the themes

### AV-2

When an analysis runs alongside a live system, its runtime overhead stays negligible because common cases are batched or short-circuited by targeted optimizations, unless every event of the run must still be monitored, in which case overhead is a multiple of the unmonitored run.

- **When:** online analysis, runtime guards or monitors attached to an executing program or transaction
- **Techniques:** interactive-debugging, runtime-verification
- **Effect:** overhead negligible (sub-percent gas, real-time on consumer hardware) versus multiplicative for full-event RV
- **Because** (paper): most invocations take a cheap path (simple invocations and deployer transactions hit dedicated optimizations; tool/language sides batch communication), so the expensive check runs rarely
- **Unless:** the monitor must observe all events of a test run (RV of Java tests: 5.8-6.4x relative to tests without RV even for the best configuration)
- **Stated:** reader · **Interesting:** yes — cross-area; the condition (common case short-circuited vs every event monitored) separates negligible from multiplicative overhead
- **Papers (3; formal, testing):** enforcing_control_flow_integrity_on_, fine_grained_analyses_for_evolution_, online_and_interactive_bayesian_infe
- **Instances (2):** testing/online_and_interactive_bayesian_infe:113, formal/enforcing_control_flow_integrity_on_:191
- **Counter-instances (2):** formal/fine_grained_analyses_for_evolution_:113, formal/fine_grained_analyses_for_evolution_:120
- **Stem:** analysis-and-verification | proposed | cost

```
improve(cost(overhead, approach=analysis-and-verification, mode=online))
  because: share(invocations, fast-path|batched) = high → checks(expensive) = rare → overhead(runtime) = negligible
  unless: overhead(runtime) = multiplicative | monitored-events=all
```

### AV-4

When the property checked is generic and fixed in advance (memory safety, control-flow whitelisting policies), setup needs little project-specific knowledge (no training transactions, no project-wide understanding) because the rules come from the property rather than from the project, unless false-positive rates must match learned approaches, which then requires adding training traces or administrator feedback.

- **When:** analysis of a predefined generic property instead of project-specific learned or full-semantic specifications
- **Techniques:** bounded-model-checking, runtime-verification
- **Effect:** setup cost (training data, project knowledge, modeling) decreases
- **Because** (paper): rules(security) = predefined policies -> training data = 0; knowledge(project-wide) minimal -> short isolated proof time; memory safety needs only return-value models (paper because lines)
- **Unless:** without training data, false positives stay above learned invariants (1.19% vs 0.23%) until training splits or feedback are added (0.15%); modeling still dominates unit-proof time
- **Stated:** implied · **Interesting:** yes — names the price of zero-knowledge setup (higher false positives until knowledge is added), a condition that changes the quality direction
- **Papers (2; formal):** do_unit_proofs_work_an_empirical_stu, enforcing_control_flow_integrity_on_
- **Instances (6):** formal/do_unit_proofs_work_an_empirical_stu:125, formal/do_unit_proofs_work_an_empirical_stu:152, formal/enforcing_control_flow_integrity_on_:174, formal/enforcing_control_flow_integrity_on_:119, formal/enforcing_control_flow_integrity_on_:120, formal/enforcing_control_flow_integrity_on_:168
- **Counter-instances (2):** formal/enforcing_control_flow_integrity_on_:169, formal/do_unit_proofs_work_an_empirical_stu:142
- **Stem:** analysis-and-verification | proposed | cost

```
improve(cost(setup, approach=analysis-and-verification, property=generic-predefined))
  because: rules(property) = predefined → training-data|project-knowledge = minimal → cost(setup) = low
  unless: fp-rate(benign) > learned-invariants | training-data=0, feedback=absent
```

### AV-6

When an analysis takes its targets from external artefacts (existing tests, identified protected contracts, administrator whitelist feedback), what it covers is bounded by those artefacts because it can only check what they expose, so sparser tests shrink scenario coverage and slower feedback raises false positives even while per-target success stays unchanged, unless per-target success on the first attempt is also measured, where sparse inputs do lower it.

- **When:** verification or guarding driven by existing tests, contract lists or operator feedback
- **Techniques:** bounded-model-checking, runtime-verification
- **Effect:** coverage and false-positive rate depend on artefact richness and freshness; success rate within covered targets is insensitive
- **Because** (reader): the tool enumerates targets from the artefacts, so missing tests, contracts or feedback remove targets or leave benign flows unwhitelisted rather than lowering per-target success
- **Unless:** first-round success under sparse tests drops (72.9% vs 80.6%) before repair rounds recover it
- **Stated:** implied · **Interesting:** yes — denies the default that a high success rate means broad coverage: success is flat in test count while coverage falls; common mechanism across harness generation and runtime guarding
- **Papers (2; formal):** enforcing_control_flow_integrity_on_, harnessllm_rust_verification_harness
- **Instances (8):** formal/harnessllm_rust_verification_harness:105, formal/harnessllm_rust_verification_harness:106, formal/harnessllm_rust_verification_harness:107, formal/harnessllm_rust_verification_harness:110, formal/harnessllm_rust_verification_harness:161, formal/enforcing_control_flow_integrity_on_:125, formal/enforcing_control_flow_integrity_on_:165, formal/enforcing_control_flow_integrity_on_:167
- **Counter-instances (1):** formal/harnessllm_rust_verification_harness:108
- **Stem:** analysis-and-verification | proposed | effectiveness

```
bound(effectiveness(coverage, approach=analysis-and-verification, targets=from-artefacts))
  because: targets(analysis) = derived-from artefacts → coverage(scenarios|flows) increasing-in artefact-richness → success-rate(per-target) independent-of artefact-richness
  unless: success-rate(rounds=0) decreasing-in artefact-sparsity
```

### DT-4

When mutation testing adds operators or code scopes that established tools lack (faults found from runtime traces, verification-language operators, small functions and return expressions), it exposes weaknesses in tests and specifications that established operators and high coverage both miss. The likely reason is that existing suites and specs were hardened against the classic fault model. This does not hold when per-operator weakness counts are read as operator potency, because those counts mostly track how many mutants an operator generates.

- **When:** the mutation tool adds operators or scopes outside the classic operator set
- **Techniques:** mutation-testing
- **Effect:** reveals more test inadequacies and specification weaknesses; the new operators' mutants are killed less often than those of adapted classic operators
- **Because** (reader): reader: suites and specs are hardened against classic mutants, so faults outside that model survive. Paper (for the caveat): mutants(generated, per-operator) increasing-in targets → mutants(alive) ∝ mutants-generated → weaknesses revealed increasing-in mutant-count
- **Unless:** weakness counts are compared across operators without normalizing for mutant volume; the assumption that mutant placement over covered locations is independent of killability does not hold
- **Stated:** implied · **Interesting:** yes — Denies the belief that high coverage means adequate tests (faults were missed even in high-coverage projects). Holds for both Python test suites and Dafny specifications. Also warns that operator effectiveness rankings are confounded by mutant volume.
- **Papers (2; testing):** hybrid_fault_driven_mutation_testing, mutdafny_a_mutation_based_approach_t
- **Instances (3):** testing/hybrid_fault_driven_mutation_testing:41, testing/hybrid_fault_driven_mutation_testing:96, testing/mutdafny_a_mutation_based_approach_t:89
- **Counter-instances (1):** testing/mutdafny_a_mutation_based_approach_t:137
- **Stem:** dynamic-testing | proposed | effectiveness

```
improve(effectiveness(weaknesses-revealed, approach=dynamic-testing, operators=novel))
  because: hardening(tests, specs) = classic-fault-model → mutants(novel-operator, killed) < mutants(classic-operator, killed) → weaknesses(revealed) increasing-in novel-operators
  unless: weaknesses(revealed, per-operator) ∝ mutant-count | placement(mutants, covered-locations) dependent-on killability
```

### DT-6

When a dynamic tester adds analysis to generate or guide candidates, total cost stays close to the unaided tool's, because run time is dominated by running and checking each candidate (resolving and verifying mutants, running tests per mutant), unless the analysis runs inside every execution (hooking all RAM, watching list-based multi-page DMA buffers, resetting when models change late); then overhead grows with the share of execution that is instrumented.

- **When:** the added analysis runs once or only on demand, outside the per-candidate execution loop
- **Techniques:** mutation-testing, greybox-fuzzing
- **Effect:** runtime per mutant is comparable to the established tool; generation is about 5% of mutation runtime; hook throughput cost is -8% to +7%
- **Because** (paper): runtime(plugin, scan-and-mutate) ∈ [4,17] s → share(generation) ≈ 5% → resolution/verification dominates; hooks(ram) = none-until-dma-found → only MMIO is hooked → throughput unchanged; assume overhead increasing-in share-of-memory-monitored
- **Unless:** instrumentation runs on every execution (all RAM hooked: -35% to -72% throughput), or models are identified late and buffers are list-based and span several pages (≈4× slower time-to-bug than the manual oracle)
- **Stated:** implied · **Interesting:** yes — The mechanism (per-candidate execution dominates cost) comes from the papers. The condition that turns cheap analysis expensive is where it sits, inside or outside the execution loop, and dyma measures both sides.
- **Papers (3; testing):** dyma_fuzz_dynamic_direct_memory_acce, hybrid_fault_driven_mutation_testing, mutdafny_a_mutation_based_approach_t
- **Instances (4):** testing/mutdafny_a_mutation_based_approach_t:160, testing/mutdafny_a_mutation_based_approach_t:159, testing/hybrid_fault_driven_mutation_testing:113, testing/dyma_fuzz_dynamic_direct_memory_acce:180
- **Counter-instances (2):** testing/dyma_fuzz_dynamic_direct_memory_acce:185, testing/dyma_fuzz_dynamic_direct_memory_acce:186
- **Stem:** dynamic-testing | proposed | cost

```
bound(cost(runtime, approach=dynamic-testing, analysis=added))
  because: runtime(generation, analysis) ≪ runtime(execution, per-candidate) → cost(total) ≈ cost(unaided)
  because: hooks(ram) = none-until-needed → overhead(throughput) ≈ 0
  unless: instrumentation(placement) = every-execution → overhead(throughput) increasing-in share-of-execution-monitored
```

### DT-9

When a dynamic tester derives its work items from the subject (mutation candidates, generated mutants, inferred DMA buffer sizes), how many or how large they are is capped by what its front-end can extract: how often a syntax construct appears, whether runtime traces are collected, whether debug symbols survive. Stripping symbols or using static analysis alone shrinks the work set, unless the subject's constructs can be found without that extra information.

- **When:** work items are extracted from the subject's code, traces or binary metadata
- **Techniques:** mutation-testing, greybox-fuzzing
- **Effect:** counts of mutants and candidates, and inferred buffer sizes, vary with the operator's syntactic frequency, the analysis mode (static vs runtime) and the availability of symbols
- **Because** (reader): targets(operator, syntax) = common → mutants(generated, per-operator) increasing-in targets; reader: static analysis sees only syntactic patterns (2 of 7 PyTation operators), while the rest need runtime traces; Ghidra's size inference needs type and symbol information
- **Unless:** binaries whose buffer sizes can be recovered without symbols (some are unaffected by stripping); operators whose targets are purely syntactic
- **Stated:** implied · **Interesting:** yes — The condition (symbols stripped, static-only mode, rare syntax) changes how much the tool can act on. That explains variation in scale across subjects that raw counts would attribute to the tool.
- **Papers (3; testing):** dyma_fuzz_dynamic_direct_memory_acce, hybrid_fault_driven_mutation_testing, mutdafny_a_mutation_based_approach_t
- **Instances (3):** testing/dyma_fuzz_dynamic_direct_memory_acce:192, testing/hybrid_fault_driven_mutation_testing:36, testing/mutdafny_a_mutation_based_approach_t:104
- **Stem:** dynamic-testing | proposed | scale

```
bound(scale(work-items, approach=dynamic-testing))
  because: targets(operator, syntax) = common → mutants(generated) increasing-in targets
  because: information(front-end, subject) ∈ {syntax, runtime-traces, debug-symbols} → work-items(extracted) increasing-in information
  unless: recoverable(work-items, without-extra-information) = yes
```

### NH-10

When predicting developer outcomes (review acceptance and velocity, intention to leave), the specific lever a study foregrounds (the review assignment process, reviewer expertise, job satisfaction alone) explains little, while broader contextual attachment and workload factors carry most of the explanatory weight (embeddedness explains at least as much turnover as satisfaction, and models adding it roughly double or triple R squared), because outcomes arise from the whole social and technical context rather than one process knob.

- **When:** explanatory models of developer outcomes with a focal organisational factor among others
- **Techniques:** none (phenomenon: predictors of developer outcomes), group vs individual review requests, turnover intention models
- **Effect:** focal lever has low importance; contextual predictors dominate and raise explained variance
- **Because** (reader): outcomes are over-determined by context (change size, discussion, workload, relationships; embeddedness in team and community), so a single process variable is swamped
- **Unless:** none stated
- **Stated:** implied · **Interesting:** yes — Denies the default belief that the process or attitude a study targets (assignment mechanism, satisfaction) is the main driver of the outcome.
- **Papers (2; human):** group_versus_individual_review_requests, staying_or_leaving_how_job_satisfaction
- **Instances (5):** human/group_versus_individual_review_requests:31, human/group_versus_individual_review_requests:53, human/staying_or_leaving_how_job_satisfaction:42, human/staying_or_leaving_how_job_satisfaction:108, human/staying_or_leaving_how_job_satisfaction:109
- **Stem:** none | none | association

```
explain(outcome(developer, approach=none), context) > explain(outcome(developer), lever)
  because: outcome(developer) depends-on {size, workload, relationships, embeddedness} → importance(lever) = low
  unless: none
```

### NH-2

When a developer phenomenon is re-measured in a different population (industrial versus earlier open-source respondents, iOS versus Android developers, practitioners versus the research literature), its prevalence and salience shift while the coarse themes replicate, because what people encounter and prioritise is shaped by their working context and by the sampling frame, unless the attribute is a top-ranked concern shared by everyone (accuracy for LLM tools), which stays invariant across sub-populations.

- **When:** the same developer practice or belief measured in two populations or sampling frames
- **Techniques:** none (phenomenon: developer practice and belief), survey studies, literature review vs practitioner survey
- **Effect:** prevalence/salience differs between populations; qualitative themes replicate
- **Because** (reader): exposure and priorities depend on work context (platform ecosystem, industrial setting, operational security stakes) and on who is sampled; the literature abstracts away operational concerns that practitioners face
- **Unless:** top-ranked, universal criteria (accuracy first regardless of task or LLM experience) do not shift
- **Stated:** implied · **Interesting:** yes — Cross-area pattern with a condition (universal top criteria are invariant, secondary salience shifts); denies the default that literature-derived or prior-sample prevalence transfers to a new population.
- **Papers (3; ai4se, human):** mapping_the_trust_terrain_llms_in_so, negativity_in_self_admitted_technical_d, practitioner_views_on_mobile_app_access
- **Instances (5):** human/negativity_in_self_admitted_technical_d:51, human/practitioner_views_on_mobile_app_access:67, human/practitioner_views_on_mobile_app_access:123, ai4se/mapping_the_trust_terrain_llms_in_so:76, ai4se/mapping_the_trust_terrain_llms_in_so:82
- **Counter-instances (3):** ai4se/mapping_the_trust_terrain_llms_in_so:108, ai4se/mapping_the_trust_terrain_llms_in_so:109, ai4se/mapping_the_trust_terrain_llms_in_so:111
- **Stem:** none | none | prevalence

```
differ(prevalence(practice, approach=none, pop=A), prevalence(practice, approach=none, pop=B))
  because: context(work, pop) differs → exposure(phenomenon) differs → prevalence(practice) depends-on pop
  unless: rank(criterion) = top → prevalence(criterion) independent-of pop
```

### NH-9

When developers' affective signals or working conditions (negativity in technical-debt comments, work-life balance, organisational justice) influence their decisions (prioritising debt, intending to leave), the influence runs through an intermediate perception or attachment (believing negativity communicates priority; job satisfaction and embeddedness), because the signal only matters to the extent it changes that intermediate state, unless the developer lacks the link, in which case the effect is null (developers who disagree that negativity signals priority; no direct work-life effect on turnover).

- **When:** developer decisions about technical-debt priority or staying in a job
- **Techniques:** none (phenomenon: developer perception and attitudes), self-admitted technical debt prioritisation, turnover intention
- **Effect:** effects are conditional or mediated: present via the intermediate perception, null as a direct or unlinked effect
- **Because** (reader): signal -> intermediate perception/attachment (priority belief; satisfaction, embeddedness) -> decision; absent the intermediate state the signal carries no decision weight
- **Unless:** developer does not hold the linking perception (odds ratio about 1) or the path is tested only as a direct effect (work-life balance H2 not supported)
- **Stated:** implied · **Interesting:** yes — Condition changes the direction (effect vs null) and denies the default belief that work-life balance or emotional tone act directly on developer decisions.
- **Papers (2; human):** negativity_in_self_admitted_technical_d, staying_or_leaving_how_job_satisfaction
- **Instances (7):** human/negativity_in_self_admitted_technical_d:19, human/negativity_in_self_admitted_technical_d:36, human/staying_or_leaving_how_job_satisfaction:37, human/staying_or_leaving_how_job_satisfaction:40, human/staying_or_leaving_how_job_satisfaction:46, human/staying_or_leaving_how_job_satisfaction:47, human/staying_or_leaving_how_job_satisfaction:51
- **Counter-instances (2):** human/negativity_in_self_admitted_technical_d:33, human/staying_or_leaving_how_job_satisfaction:43
- **Stem:** none | none | perception

```
increase(decision(developer, approach=none), signal) via perception(link)
  because: signal → perception(link) → decision(developer) depends-on perception(link)
  unless: perception(link) = absent → decision(developer) independent-of signal
```


## Laws marked not interesting

### AV-13

When per-item cost is reported, proposed analysis tools run within practical bounds (under a second and a few hundred KB per loop, under a minute for most unit proofs, cents per harness) because scope per item is small, unless a reasoning-heavy LLM is used, which is an order of magnitude slower.

- **When:** per-item run cost of a proposed analysis or verification tool
- **Techniques:** symbolic-execution, bounded-model-checking
- **Effect:** per-item time, memory and dollar cost low
- **Because** (paper): minimal unwinding bounds limit unit scope; reasoning models spend many tokens on reasoning steps (paper because lines)
- **Unless:** reasoning LLM (DS-R1) takes 1526 s per harness vs 145 s for the fastest
- **Stated:** yes · **Interesting:** no
- **Papers (3; formal):** do_unit_proofs_work_an_empirical_stu, harnessllm_rust_verification_harness, loopscc_summarizing_complex_multi_br
- **Instances (7):** formal/loopscc_summarizing_complex_multi_br:166, formal/loopscc_summarizing_complex_multi_br:167, formal/do_unit_proofs_work_an_empirical_stu:147, formal/do_unit_proofs_work_an_empirical_stu:150, formal/harnessllm_rust_verification_harness:136, formal/harnessllm_rust_verification_harness:142, formal/harnessllm_rust_verification_harness:143
- **Counter-instances (1):** formal/harnessllm_rust_verification_harness:139
- **Stem:** analysis-and-verification | proposed | cost

```
bound(cost(time|memory|usd, per-item, approach=analysis-and-verification))
  because: scope(item) = small → cost(per-item) = low
  unless: cost(per-item) = high | llm=reasoning
```

### AV-14

When evaluated on their own benchmarks, proposed analysis tools beat their baselines (issues solved, attacks blocked, harness success, proof coverage, projects won), unless the task is simple or the baseline is already selective, where gains shrink or vanish.

- **When:** proposed tool vs its chosen baselines on the paper's own effectiveness metric
- **Techniques:** interactive-debugging, runtime-verification, bounded-model-checking
- **Effect:** effectiveness better than baselines
- **Because** (none): none stated
- **Unless:** simplest task (no significant difference); already-fast Ps3cl baseline (13/68 and 2/68 wins); combined use with competitor asserted, not evaluated
- **Stated:** yes · **Interesting:** no
- **Papers (5; formal, testing):** do_unit_proofs_work_an_empirical_stu, enforcing_control_flow_integrity_on_, fine_grained_analyses_for_evolution_, harnessllm_rust_verification_harness, online_and_interactive_bayesian_infe
- **Instances (13):** testing/online_and_interactive_bayesian_infe:38, testing/online_and_interactive_bayesian_infe:40, formal/enforcing_control_flow_integrity_on_:119, formal/enforcing_control_flow_integrity_on_:120, formal/harnessllm_rust_verification_harness:86, formal/harnessllm_rust_verification_harness:111, formal/do_unit_proofs_work_an_empirical_stu:94, formal/do_unit_proofs_work_an_empirical_stu:182, formal/fine_grained_analyses_for_evolution_:55, formal/fine_grained_analyses_for_evolution_:56, formal/fine_grained_analyses_for_evolution_:57, formal/fine_grained_analyses_for_evolution_:83, formal/fine_grained_analyses_for_evolution_:87
- **Counter-instances (4):** testing/online_and_interactive_bayesian_infe:42, formal/fine_grained_analyses_for_evolution_:67, formal/fine_grained_analyses_for_evolution_:68, formal/enforcing_control_flow_integrity_on_:124
- **Stem:** analysis-and-verification | proposed | effectiveness

```
improve(effectiveness(success|blocked|coverage|wins, approach=analysis-and-verification))
  unless: effectiveness = parity | task=simple, baseline=already-selective
```

### AV-15

When evaluated on their own ground truth, proposed analysis tools report higher accuracy or recall and lower false-positive rates than baselines, unless trained competitors are compared without training data or loops need nonlinear closed forms.

- **When:** proposed tool vs baselines on accuracy, recall or false-positive rate
- **Techniques:** static-analysis, runtime-verification, symbolic-execution
- **Effect:** quality better than baselines
- **Because** (none): none stated
- **Unless:** CrossGuard without training data has FP 1.19% above Trace2Inv's 0.23%; LoopSCC fails on nonlinear loops
- **Stated:** yes · **Interesting:** no
- **Papers (3; formal, testing):** enforcing_control_flow_integrity_on_, loopscc_summarizing_complex_multi_br, precise_static_identification_of_eth
- **Instances (8):** testing/precise_static_identification_of_eth:63, testing/precise_static_identification_of_eth:64, testing/precise_static_identification_of_eth:70, formal/enforcing_control_flow_integrity_on_:168, formal/loopscc_summarizing_complex_multi_br:79, formal/loopscc_summarizing_complex_multi_br:90, formal/loopscc_summarizing_complex_multi_br:111, formal/loopscc_summarizing_complex_multi_br:112
- **Counter-instances (1):** formal/enforcing_control_flow_integrity_on_:169
- **Stem:** analysis-and-verification | proposed | quality

```
improve(quality(accuracy|recall|fp-rate, approach=analysis-and-verification))
  unless: fp-rate > baseline | training-data=0; accuracy = low | loop=nonlinear
```

### AV-16

When arguing reach, analysis papers report scale of use or of targets (hundreds of thousands of affected contract deployments, thousands of users, proved functions larger and more complex than typical), with no condition stated.

- **When:** reach or representativeness claims for a proposed analysis
- **Techniques:** static-analysis, bounded-model-checking
- **Effect:** counts of deployments, adopters or target size reported as large
- **Because** (paper): low-level declarations leave compiler storage metadata incomplete, so many deployed contracts need the analysis (paper because line)
- **Unless:** none stated
- **Stated:** yes · **Interesting:** no
- **Papers (2; formal, testing):** do_unit_proofs_work_an_empirical_stu, precise_static_identification_of_eth
- **Instances (5):** testing/precise_static_identification_of_eth:92, testing/precise_static_identification_of_eth:103, testing/precise_static_identification_of_eth:104, formal/do_unit_proofs_work_an_empirical_stu:194, formal/do_unit_proofs_work_an_empirical_stu:195
- **Stem:** analysis-and-verification | proposed | scale

```
more(scale(count(deployments|adopters)|size(targets), approach=analysis-and-verification))
  because: metadata(storage-layout) = incomplete → count(contracts, needing-analysis) = large
```

### AV-17

When presenting a new analysis, papers decompose the problem into tractable parts (SCC contraction and per-SCC summaries for loops, per-invocation call-trace hashing against a whitelist, heuristic warnings that guide users to subsets of the inference state) because the full exploration space is too large, with no condition stated.

- **When:** design descriptions of proposed analysis tools
- **Techniques:** interactive-debugging, runtime-verification, symbolic-execution
- **Effect:** design asserted
- **Because** (paper): large exploration space -> heuristic warnings guide to subsets -> faster debugging (paper because line)
- **Unless:** none stated
- **Stated:** yes · **Interesting:** no
- **Papers (3; formal, testing):** enforcing_control_flow_integrity_on_, loopscc_summarizing_complex_multi_br, online_and_interactive_bayesian_infe
- **Instances (6):** testing/online_and_interactive_bayesian_infe:32, testing/online_and_interactive_bayesian_infe:36, formal/enforcing_control_flow_integrity_on_:84, formal/loopscc_summarizing_complex_multi_br:49, formal/loopscc_summarizing_complex_multi_br:50, formal/loopscc_summarizing_complex_multi_br:51
- **Stem:** analysis-and-verification | proposed | design

```
assert(design(technique, approach=analysis-and-verification, decomposition=on))
  because: space(exploration) = large → decomposition(problem) = parts → analysis(parts) = tractable
```

### DT-11

When evaluated on their own benchmarks, proposed dynamic testers (DMA-aware firmware fuzzing, misbehaviour-forecasting ADS testing, interaction-aware greybox fuzzing) find more coverage or failures than their baselines, unless the effect is small or bug totals tie.

- **When:** the proposed tool is evaluated on the authors' chosen benchmarks against the authors' chosen baselines
- **Techniques:** greybox-fuzzing, simulation-testing
- **Effect:** more coverage, failures, collisions or bugs than the baselines
- **Because** (none): none stated
- **Unless:** small or very small effect sizes (Transfuser); bug totals tie (MuoFuzz vs MOPT); unique bugs per trial are not significant
- **Stated:** implied · **Interesting:** no — Trite: the proposed tool beats its baselines on its own metric.
- **Papers (3; testing):** dyma_fuzz_dynamic_direct_memory_acce, misbehavior_forecasting_for_focused, on_interaction_effects_in_greybox_fu
- **Instances (18):** testing/dyma_fuzz_dynamic_direct_memory_acce:91, testing/dyma_fuzz_dynamic_direct_memory_acce:165, testing/misbehavior_forecasting_for_focused:36, testing/misbehavior_forecasting_for_focused:38, testing/misbehavior_forecasting_for_focused:39, testing/misbehavior_forecasting_for_focused:44, testing/misbehavior_forecasting_for_focused:55, testing/misbehavior_forecasting_for_focused:74, testing/misbehavior_forecasting_for_focused:75, testing/misbehavior_forecasting_for_focused:76, testing/misbehavior_forecasting_for_focused:77, testing/misbehavior_forecasting_for_focused:89, testing/misbehavior_forecasting_for_focused:91, testing/misbehavior_forecasting_for_focused:92, testing/misbehavior_forecasting_for_focused:94, testing/misbehavior_forecasting_for_focused:95, testing/on_interaction_effects_in_greybox_fu:62, testing/on_interaction_effects_in_greybox_fu:127
- **Counter-instances (3):** testing/misbehavior_forecasting_for_focused:42, testing/on_interaction_effects_in_greybox_fu:131, testing/on_interaction_effects_in_greybox_fu:136
- **Stem:** dynamic-testing | proposed | effectiveness

```
improve(effectiveness(failures, approach=dynamic-testing, subject-role=proposed))
  because: none stated
  unless: effect-size = small | bugs(total) = tie
```

### DT-12

Mutation analysis cost per project is claimed to scale poorly to multi-file projects and to be offset by parallel execution, probably because per-mutant cost multiplied by mutant volume grows with project size, but no paper measures either the scaling or the mitigation.

- **When:** multi-file projects
- **Techniques:** mutation-testing
- **Effect:** runtime per project is claimed to grow beyond acceptable levels; parallelism is claimed to mitigate it
- **Because** (reader): reader: runtime(project) ≈ runtime(per-mutant) × mutants, and both grow with project size
- **Unless:** parallel execution of scanner, mutator and tests is enabled (asserted, not measured)
- **Stated:** implied · **Interesting:** no — Not interesting: both instances are assertions without measurement, so the law restates an untested concern.
- **Papers (2; testing):** hybrid_fault_driven_mutation_testing, mutdafny_a_mutation_based_approach_t
- **Instances (2):** testing/hybrid_fault_driven_mutation_testing:116, testing/mutdafny_a_mutation_based_approach_t:162
- **Stem:** dynamic-testing | proposed | cost

```
bound(cost(runtime, approach=dynamic-testing, scope=multi-file))
  because: runtime(project) ≈ runtime(per-mutant) × count(mutants) → runtime(project) increasing-in project-size
  unless: parallelism(scanner, mutator, tests) = on
```

### DT-13

Guided dynamic testers observe recurring regularities in their subjects: failure rates per risky point rise with the number of sampled risky points, and learned mutator orderings (chunk after unit) recur across most programs. No mechanism is given.

- **When:** focused sampling or learned mutation scheduling on the tool's benchmark subjects
- **Techniques:** simulation-testing, greybox-fuzzing
- **Effect:** failure rate per risky point increases with nrp; the same next-mutator probability pattern appears in 9 of 13 programs
- **Because** (none): none stated
- **Unless:** subject-specific exceptions (openssl, libpng, libpcap orderings)
- **Stated:** implied · **Interesting:** no — Trite: the studies only observe prevalence.
- **Papers (2; testing):** misbehavior_forecasting_for_focused, on_interaction_effects_in_greybox_fu
- **Instances (4):** testing/misbehavior_forecasting_for_focused:59, testing/misbehavior_forecasting_for_focused:60, testing/misbehavior_forecasting_for_focused:62, testing/on_interaction_effects_in_greybox_fu:174
- **Counter-instances (2):** testing/on_interaction_effects_in_greybox_fu:176, testing/on_interaction_effects_in_greybox_fu:178
- **Stem:** dynamic-testing | proposed | prevalence

```
more(prevalence(regularity, approach=dynamic-testing))
  because: none stated
  unless: subject = exception
```

### LB-A-15

Proposed learning-based tools are reported as faster, cheaper or lighter than their baselines on their own cost metrics (latency, reviewer workload, edit time, throughput), usually with no mechanism given beyond the design itself.

- **When:** a proposed learning-based tool is compared with its baselines on a cost metric it chose
- **Techniques:** learned-ranking, model-editing, deobfuscation, llm-prompting, probing
- **Effect:** lower latency, workload or time than baselines
- **Because** (none): none stated
- **Unless:** one weighting raised workload (RevRecWL[meetings] top-3); LAT adds 0.2 s per task over RankEF, amortised against RankEF's setup cost
- **Stated:** implied · **Interesting:** no — Trite: the proposed tool beats its baselines on its own cost metric.
- **Papers (4; ai4se, human):** cascade_llm_powered_javascript_deobf, creme_robustness_enhancement_of_code, improving_code_reviewer_recommendation, on_llms_internal_representation_of_c
- **Instances (15):** human/improving_code_reviewer_recommendation:45, human/improving_code_reviewer_recommendation:49, human/improving_code_reviewer_recommendation:75, human/improving_code_reviewer_recommendation:76, human/improving_code_reviewer_recommendation:77, human/improving_code_reviewer_recommendation:78, human/improving_code_reviewer_recommendation:80, human/improving_code_reviewer_recommendation:81, human/improving_code_reviewer_recommendation:82, ai4se/creme_robustness_enhancement_of_code:150, ai4se/creme_robustness_enhancement_of_code:151, ai4se/creme_robustness_enhancement_of_code:152, ai4se/cascade_llm_powered_javascript_deobf:47, ai4se/cascade_llm_powered_javascript_deobf:116, ai4se/on_llms_internal_representation_of_c:193
- **Counter-instances (2):** human/improving_code_reviewer_recommendation:83, ai4se/on_llms_internal_representation_of_c:199
- **Stem:** learning-based | proposed | cost

```
reduce(cost(latency, approach=learning-based, tool=proposed))
```

### LB-A-16

Proposed learning-based tools report higher accuracy, F1, MCC or semantic accuracy than their baselines on their own benchmarks, with occasional single-cell losses.

- **When:** a proposed learning-based tool is compared with its baselines on a quality metric and benchmark it chose
- **Techniques:** learned-ranking, llm-prompting, llm-fine-tuning, translation, probing, llm-as-judge, spec-inference
- **Effect:** higher quality scores than baselines
- **Because** (none): none stated
- **Unless:** isolated cells where a baseline wins (Hua at theta=0.1; FAST++ and field-ready on one recall index; RankEF for CodeLlama at rank 1)
- **Stated:** implied · **Interesting:** no — Trite: the proposed tool beats its baselines on its own metric.
- **Papers (6; ai4se, formal, human, testing):** e_test_e_er_improving_test_suites, evoc2rust_a_skeleton_guided_framewor, hoareprompt_structural_reasoning_abo, improving_code_reviewer_recommendation, on_llms_internal_representation_of_c, toxicity_ahead_forecasting_conversation
- **Instances (17):** human/improving_code_reviewer_recommendation:36, human/improving_code_reviewer_recommendation:39, human/toxicity_ahead_forecasting_conversation:29, human/toxicity_ahead_forecasting_conversation:30, human/toxicity_ahead_forecasting_conversation:35, human/toxicity_ahead_forecasting_conversation:38, testing/e_test_e_er_improving_test_suites:65, testing/e_test_e_er_improving_test_suites:68, testing/e_test_e_er_improving_test_suites:75, ai4se/evoc2rust_a_skeleton_guided_framewor:93, ai4se/evoc2rust_a_skeleton_guided_framewor:94, ai4se/evoc2rust_a_skeleton_guided_framewor:98, ai4se/on_llms_internal_representation_of_c:67, ai4se/on_llms_internal_representation_of_c:96, ai4se/on_llms_internal_representation_of_c:121, ai4se/on_llms_internal_representation_of_c:136, formal/hoareprompt_structural_reasoning_abo:130
- **Counter-instances (4):** human/toxicity_ahead_forecasting_conversation:34, testing/e_test_e_er_improving_test_suites:92, testing/e_test_e_er_improving_test_suites:93, ai4se/on_llms_internal_representation_of_c:132
- **Stem:** learning-based | proposed | quality

```
improve(quality(accuracy, approach=learning-based, tool=proposed))
```

### LB-A-17

Proposed learning-based tools report higher compile, coverage, pass@1, detection or proof success than their baselines on their own benchmarks, with occasional per-dataset losses.

- **When:** a proposed learning-based tool is compared with its baselines on an effectiveness metric and benchmark it chose
- **Techniques:** test-generation, model-editing, translation, proof-synthesis, learned-ranking
- **Effect:** higher effectiveness than baselines
- **Because** (none): none stated
- **Unless:** per-dataset losses (CREME to Self-Denoising on QwenCoder+MBPP C3; Cobblestone to PALM on CoqGym100; EvoC2Rust to C2Rust on compile rate)
- **Stated:** implied · **Interesting:** no — Trite: the proposed tool beats its baselines on its own metric.
- **Papers (7; ai4se, formal, human, testing):** are_solved_issues_in_swe_bench_reall, citywalk_enhancing_llm_based_c_unit, cobblestone_a_divide_and_conquer_app, creme_robustness_enhancement_of_code, evoc2rust_a_skeleton_guided_framewor, improving_code_reviewer_recommendation, proofcoop_collaborative_automated_fo
- **Instances (18):** testing/citywalk_enhancing_llm_based_c_unit:85, testing/citywalk_enhancing_llm_based_c_unit:103, testing/citywalk_enhancing_llm_based_c_unit:119, testing/citywalk_enhancing_llm_based_c_unit:122, ai4se/creme_robustness_enhancement_of_code:56, ai4se/creme_robustness_enhancement_of_code:62, ai4se/creme_robustness_enhancement_of_code:63, ai4se/evoc2rust_a_skeleton_guided_framewor:74, ai4se/evoc2rust_a_skeleton_guided_framewor:75, ai4se/evoc2rust_a_skeleton_guided_framewor:104, formal/cobblestone_a_divide_and_conquer_app:81, formal/cobblestone_a_divide_and_conquer_app:91, formal/cobblestone_a_divide_and_conquer_app:99, ai4se/are_solved_issues_in_swe_bench_reall:127, ai4se/are_solved_issues_in_swe_bench_reall:130, human/improving_code_reviewer_recommendation:57, formal/proofcoop_collaborative_automated_fo:105, formal/proofcoop_collaborative_automated_fo:107
- **Counter-instances (3):** ai4se/creme_robustness_enhancement_of_code:60, formal/cobblestone_a_divide_and_conquer_app:94, ai4se/evoc2rust_a_skeleton_guided_framewor:71
- **Stem:** learning-based | proposed | effectiveness

```
improve(effectiveness(success-rate, approach=learning-based, tool=proposed))
```

### LB-B-19

Proposed learning-based tools are described by their feature catalogues, recommended thresholds, novelty claims, voting and caching variants, and released artefacts, with temperature reported as having no effect on performance.

- **When:** paper describes its own tool's design
- **Techniques:** learned-ranking, llm-prompting, test-generation, llm-fine-tuning, llm-as-judge, proof-synthesis
- **Effect:** design asserted
- **Because** (none): none stated
- **Unless:** voting may follow low-confidence consensus when majority accuracy is low (proofcoop:69)
- **Stated:** yes · **Interesting:** no — restates design choices
- **Papers (6; formal, human, testing):** citywalk_enhancing_llm_based_c_unit, e_test_e_er_improving_test_suites, hoareprompt_structural_reasoning_abo, improving_code_reviewer_recommendation, proofcoop_collaborative_automated_fo, toxicity_ahead_forecasting_conversation
- **Instances (9):** human/improving_code_reviewer_recommendation:138, human/toxicity_ahead_forecasting_conversation:97, testing/citywalk_enhancing_llm_based_c_unit:57, testing/citywalk_enhancing_llm_based_c_unit:59, testing/e_test_e_er_improving_test_suites:111, formal/hoareprompt_structural_reasoning_abo:48, formal/hoareprompt_structural_reasoning_abo:241, formal/proofcoop_collaborative_automated_fo:69, formal/proofcoop_collaborative_automated_fo:77
- **Stem:** learning-based | proposed | design

```
assert(design(tool, approach=learning-based))
```

### LB-B-20

Proposed learning-based provers and test generators prove more theorems uniquely or generate test sets that compare favourably with their baselines, with some complementarity with non-LLM provers because hammers cannot use induction.

- **When:** proposed tool compared with its chosen baselines on its own count metric
- **Techniques:** proof-synthesis, test-generation, spec-inference
- **Effect:** more theorems proven-only; favourable test counts; short traces
- **Because** (paper): hammers cannot use induction, a key step in many Coq proofs (cobblestone:88); different oracle information yields complementary proofs (cobblestone:212)
- **Unless:** non-LLM tools and PALM/Rango still prove theorems the proposed tool cannot (cobblestone:88, :100, :110)
- **Stated:** yes · **Interesting:** no — proposed tool beats its baselines on its own metric
- **Papers (3; formal, testing):** automating_requirements_formalizatio, citywalk_enhancing_llm_based_c_unit, cobblestone_a_divide_and_conquer_app
- **Instances (8):** formal/cobblestone_a_divide_and_conquer_app:88, formal/cobblestone_a_divide_and_conquer_app:100, formal/cobblestone_a_divide_and_conquer_app:110, formal/cobblestone_a_divide_and_conquer_app:212, formal/cobblestone_a_divide_and_conquer_app:214, testing/citywalk_enhancing_llm_based_c_unit:113, testing/citywalk_enhancing_llm_based_c_unit:114, formal/automating_requirements_formalizatio:123
- **Stem:** learning-based | proposed | scale

```
improve(scale(count(solved), approach=learning-based, tool=proposed))
  because: induction(hammer) = unsupported → count(theorem, proven-only) increasing-in induction
```

### LB-B-21

Proposed learning-based tools draw more clicks or higher human ratings than their previous versions or LLM baselines, and stay under the not-helpful guideline for posting findings.

- **When:** proposed tool compared with its chosen baselines on its own usability metric
- **Techniques:** learned-ranking, test-generation, llm-prompting
- **Effect:** clicks and ratings higher; not-helpful rate < 10%
- **Because** (none): none stated
- **Unless:** clicks also move with latency, which changed together with the model (improving_code_reviewer_recommendation:60)
- **Stated:** yes · **Interesting:** no — proposed tool beats its baselines on its own metric
- **Papers (3; ai4se, human, testing):** citywalk_enhancing_llm_based_c_unit, improving_code_reviewer_recommendation, llm_based_automated_diagnosis_of_int
- **Instances (7):** human/improving_code_reviewer_recommendation:53, human/improving_code_reviewer_recommendation:60, testing/citywalk_enhancing_llm_based_c_unit:222, testing/citywalk_enhancing_llm_based_c_unit:224, testing/citywalk_enhancing_llm_based_c_unit:228, testing/citywalk_enhancing_llm_based_c_unit:230, ai4se/llm_based_automated_diagnosis_of_int:105
- **Stem:** learning-based | proposed | usability

```
improve(usability(clicks|ratings, approach=learning-based, tool=proposed))
```

### LB-B-22

Proposed learning-based tools assert correctness properties by construction (soundness, semantics preservation, search completeness, no execution-fix false positives) and mostly do not measure them.

- **When:** paper asserts a correctness property of its own design
- **Techniques:** deobfuscation, proof-synthesis, test-generation
- **Effect:** correctness asserted, unmeasured
- **Because** (none): none stated
- **Unless:** none stated
- **Stated:** yes · **Interesting:** no — restates design choices
- **Papers (4; ai4se, formal, testing):** cascade_llm_powered_javascript_deobf, citywalk_enhancing_llm_based_c_unit, cobblestone_a_divide_and_conquer_app, proofcoop_collaborative_automated_fo
- **Instances (4):** ai4se/cascade_llm_powered_javascript_deobf:129, formal/cobblestone_a_divide_and_conquer_app:55, formal/proofcoop_collaborative_automated_fo:68, testing/citywalk_enhancing_llm_based_c_unit:82
- **Stem:** learning-based | proposed | correctness

```
assert(correctness(tool, approach=learning-based))
```

### LB-B-23

Baselines cost more than the proposed learning-based tool on the proposed tool's cost metric (hand-written rule lines, validation traces, search time).

- **When:** baseline compared with the proposed tool on the proposed tool's cost metric
- **Techniques:** deobfuscation, spec-inference, proof-synthesis
- **Effect:** baseline cost higher
- **Because** (none): none stated
- **Unless:** token cost not comparable across tokenizers and model sizes (cobblestone:148)
- **Stated:** yes · **Interesting:** no — proposed tool beats its baselines on its own metric
- **Papers (3; ai4se, formal):** automating_requirements_formalizatio, cascade_llm_powered_javascript_deobf, cobblestone_a_divide_and_conquer_app
- **Instances (6):** ai4se/cascade_llm_powered_javascript_deobf:84, ai4se/cascade_llm_powered_javascript_deobf:85, formal/automating_requirements_formalizatio:92, formal/automating_requirements_formalizatio:93, formal/automating_requirements_formalizatio:95, formal/cobblestone_a_divide_and_conquer_app:148
- **Stem:** learning-based | baseline | cost

```
worsen(cost(baseline, approach=learning-based)) vs tool=proposed
```

### LB-B-24

Proposed learning-based tools are reported to shift the frequency of desirable artefact properties (fewer failing tests, more mock usage, a narrower production–test execution gap) or to concentrate their effect in a few layers, mostly without a statistical test or a defined metric.

- **When:** paper reports prevalence for its own tool
- **Techniques:** test-generation, llm-fine-tuning, model-editing
- **Effect:** favourable prevalence observed or asserted
- **Because** (none): none stated
- **Unless:** none stated
- **Stated:** yes · **Interesting:** no — study observed a prevalence
- **Papers (3; ai4se, testing):** citywalk_enhancing_llm_based_c_unit, creme_robustness_enhancement_of_code, e_test_e_er_improving_test_suites
- **Instances (5):** testing/citywalk_enhancing_llm_based_c_unit:110, testing/citywalk_enhancing_llm_based_c_unit:208, testing/citywalk_enhancing_llm_based_c_unit:210, testing/e_test_e_er_improving_test_suites:173, ai4se/creme_robustness_enhancement_of_code:120
- **Stem:** learning-based | proposed | prevalence

```
observe(prevalence(property, approach=learning-based, tool=proposed))
```

### LB-B-25

The share of problematic LLM outputs and hard items varies by project and model: flaky-test shares differ across projects and models with similar flakiness causes, generated tests differ syntactically from existing ones, and most real bugs have low agent success rates.

- **When:** prevalence of LLM output properties observed across projects/models
- **Techniques:** commercial-llm, llm-agent
- **Effect:** prevalence depends on project and model
- **Because** (none): none stated
- **Unless:** concurrency flakiness differs between models (flakiness:90)
- **Stated:** yes · **Interesting:** no — study observed a prevalence
- **Papers (2; ai4se, testing):** abstain_and_validate_a_dual_llm_poli, on_the_flakiness_of_llm_generated_te
- **Instances (4):** testing/on_the_flakiness_of_llm_generated_te:49, testing/on_the_flakiness_of_llm_generated_te:90, testing/on_the_flakiness_of_llm_generated_te:123, ai4se/abstain_and_validate_a_dual_llm_poli:51
- **Stem:** learning-based | subject-model | prevalence

```
depends-on(prevalence(outputs, approach=learning-based), project | model)
```

### LB-B-26

Some subject LLMs or agents outperform others on the same task (GPT-4o over Mistral in compile success; LearnByInteract over OpenHands in resolve rate).

- **When:** subject models compared on one task
- **Techniques:** commercial-llm, llm-agent
- **Effect:** effectiveness depends on model
- **Because** (none): none stated
- **Unless:** the gap narrows under stricter validation (see LB-B-17)
- **Stated:** yes · **Interesting:** no — observed model ranking without mechanism
- **Papers (2; ai4se, testing):** are_solved_issues_in_swe_bench_reall, on_the_flakiness_of_llm_generated_te
- **Instances (4):** testing/on_the_flakiness_of_llm_generated_te:110, testing/on_the_flakiness_of_llm_generated_te:111, ai4se/are_solved_issues_in_swe_bench_reall:85, ai4se/are_solved_issues_in_swe_bench_reall:86
- **Stem:** learning-based | subject-model | effectiveness

```
depends-on(effectiveness(outputs, approach=learning-based), model)
```

### LB-B-27

The proposed model-editing tool improves robustness to prompt perturbations over editing and fine-tuning baselines, each of its components (layer localization, early stopping, preservation loss) contributes, gains vary with perturbation type and dataset, and percentile cutoffs are asserted to resist score drift.

- **When:** proposed tool compared with its baselines and ablations on its own robustness metric
- **Techniques:** model-editing, llm-as-judge
- **Effect:** G-RIR higher than LoRA/ROME/DINM; ablations lower; drift mitigation asserted
- **Because** (paper): the key layer depends on the model so a random layer restores less (creme:98); removing preservation loss leaves hidden-state deviation unconstrained (creme:104); word-level perturbations preserve semantics but shift attention (creme:76); shorter MBPP prompts are more regular (creme:80)
- **Unless:** none stated
- **Stated:** yes · **Interesting:** no — proposed tool beats its baselines and ablations on its own metric; mechanisms are single-paper
- **Papers (2; ai4se):** abstain_and_validate_a_dual_llm_poli, creme_robustness_enhancement_of_code
- **Instances (15):** ai4se/creme_robustness_enhancement_of_code:46, ai4se/creme_robustness_enhancement_of_code:65, ai4se/creme_robustness_enhancement_of_code:66, ai4se/creme_robustness_enhancement_of_code:68, ai4se/creme_robustness_enhancement_of_code:76, ai4se/creme_robustness_enhancement_of_code:77, ai4se/creme_robustness_enhancement_of_code:80, ai4se/creme_robustness_enhancement_of_code:81, ai4se/creme_robustness_enhancement_of_code:82, ai4se/creme_robustness_enhancement_of_code:98, ai4se/creme_robustness_enhancement_of_code:100, ai4se/creme_robustness_enhancement_of_code:104, ai4se/creme_robustness_enhancement_of_code:106, ai4se/creme_robustness_enhancement_of_code:107, ai4se/abstain_and_validate_a_dual_llm_poli:195
- **Stem:** learning-based | proposed | robustness

```
improve(robustness(grir, approach=learning-based, tool=proposed))
  because: layer(key-layer) depends-on model → restoration(edit, layer=random) = lower
  because: loss(preservation) = removed → deviation(hidden-state, clean) = unconstrained
```

### NH-13

Papers motivate their problem with a prevalence of pain or neglect (DeFi losses, integration-test diagnosis among top complaints, the bystander effect in review, trust literature rarely defining trustworthiness or distrust) that is cited or asserted rather than measured in the paper.

- **When:** introduction and motivation sections
- **Techniques:** none (phenomenon: motivating prevalence)
- **Effect:** asserted prevalence of a problem or gap
- **Because** (none): none stated
- **Unless:** none stated
- **Stated:** implied · **Interesting:** no — Trite: this study observed (or asserted) a prevalence.
- **Papers (4; ai4se, formal, human):** enforcing_control_flow_integrity_on_, improving_code_reviewer_recommendation, llm_based_automated_diagnosis_of_int, mapping_the_trust_terrain_llms_in_so
- **Instances (8):** formal/enforcing_control_flow_integrity_on_:44, ai4se/llm_based_automated_diagnosis_of_int:38, ai4se/mapping_the_trust_terrain_llms_in_so:69, ai4se/mapping_the_trust_terrain_llms_in_so:70, ai4se/mapping_the_trust_terrain_llms_in_so:71, ai4se/mapping_the_trust_terrain_llms_in_so:172, ai4se/mapping_the_trust_terrain_llms_in_so:173, human/improving_code_reviewer_recommendation:114
- **Stem:** none | none | prevalence

```
assert(prevalence(problem, approach=none) = high)
  because: none stated
```

### NH-14

When wall-clock cost is unreliable or not measured, papers argue cost from a structural proxy (number of test runs, prover calls per search step, dependence on existing specifications), because the proxy is proportional to the work done, and use that argument to justify a design choice.

- **When:** cost claims without controlled timing
- **Techniques:** none (phenomenon: cost proxies), runtime verification, Coq proof search, unit proofing
- **Effect:** cost asserted via proxy (steps, runs, dependencies)
- **Because** (paper): runs(tests, dynamic-analysis) = 2 -> overhead >= 2x; calls(prover, per-step) = 1 -> runtime proportional to steps
- **Unless:** none stated
- **Stated:** implied · **Interesting:** no — Trite: restates measurement and design choices.
- **Papers (3; formal):** do_unit_proofs_work_an_empirical_stu, fine_grained_analyses_for_evolution_, proofcoop_collaborative_automated_fo
- **Instances (3):** formal/do_unit_proofs_work_an_empirical_stu:49, formal/fine_grained_analyses_for_evolution_:125, formal/proofcoop_collaborative_automated_fo:114
- **Stem:** none | none | cost

```
estimate(cost(analysis, approach=none), proxy)
  because: calls(engine, per-unit) = constant → runtime ∝ proxy
  assume: runtime(wall-clock) = unreliable
```

### NH-15

When evaluations run LLMs at scale, token and dollar cost bounds the study, so prompts are sized and theorem sets subsampled to fit budget.

- **When:** LLM-based evaluation budgets
- **Techniques:** none (phenomenon: LLM evaluation cost), LLM fault localization, LLM proof synthesis
- **Effect:** evaluation cost is high and drives subsetting
- **Because** (reader): price proportional to tokens times queries times configurations
- **Unless:** none stated
- **Stated:** implied · **Interesting:** no — Trite: reports observed resource cost.
- **Papers (2; ai4se, formal):** cobblestone_a_divide_and_conquer_app, order_matters_an_empirical_study_on
- **Instances (3):** formal/cobblestone_a_divide_and_conquer_app:152, ai4se/order_matters_an_empirical_study_on:87, ai4se/order_matters_an_empirical_study_on:88
- **Stem:** none | none | cost

```
bound(size(evaluation, approach=none), cost(llm))
  because: price(llm) ∝ tokens × queries → size(evaluation) decreasing-in price
```

### NH-16

Papers contribute definitions, datasets, metrics and replication packages (infrastructure definitions, EER-improving test suites, G-RIR, GitHub-profile dataset, trust definitions, calls for group review) as instruments without evaluating them in the paper.

- **When:** contribution statements
- **Techniques:** none (phenomenon: research instruments)
- **Effect:** instrument or recommendation asserted
- **Because** (none): none stated
- **Unless:** none stated
- **Stated:** implied · **Interesting:** no — Trite: restates design choices and contributions.
- **Papers (6; ai4se, human, testing):** creme_robustness_enhancement_of_code, e_test_e_er_improving_test_suites, group_versus_individual_review_requests, mapping_the_trust_terrain_llms_in_so, once_upon_a_team_investigating_bias_in, the_software_infrastructure_attitude_sc
- **Instances (9):** human/the_software_infrastructure_attitude_sc:25, human/the_software_infrastructure_attitude_sc:26, testing/e_test_e_er_improving_test_suites:50, ai4se/creme_robustness_enhancement_of_code:51, human/once_upon_a_team_investigating_bias_in:118, human/once_upon_a_team_investigating_bias_in:122, human/group_versus_individual_review_requests:77, human/group_versus_individual_review_requests:79, ai4se/mapping_the_trust_terrain_llms_in_so:192
- **Stem:** none | none | design

```
assert(instrument(definition|dataset|metric, approach=none))
  because: none stated
```

### NH-17

Survey studies justify their samples by a power analysis or by resemblance to a reference developer population (Stack Overflow survey).

- **When:** survey sample adequacy
- **Techniques:** none (phenomenon: survey samples)
- **Effect:** sample size exceeds minimum; demographics match reference population
- **Because** (reader): adequate power and representative demographics support generalisation
- **Unless:** none stated
- **Stated:** implied · **Interesting:** no — Trite: methodological adequacy check.
- **Papers (2; human):** from_gains_to_strains_modeling_develop, negativity_in_self_admitted_technical_d
- **Instances (2):** human/from_gains_to_strains_modeling_develop:79, human/negativity_in_self_admitted_technical_d:60
- **Stem:** none | none | scale

```
satisfy(sample(survey, approach=none), power ∧ representativeness)
  because: sample ≥ minimum(power) ∧ demographics ≈ reference → generalisation = supported
```

### NH-18

Validity is claimed by meeting conventional thresholds (loadings >= 0.70, VIF < 3) or by indirect argument (training-data leakage dismissed by a post-cutoff project gap) rather than by a direct test, with some checks reported as imperfect but stable.

- **When:** threats-to-validity and measurement-model checks
- **Techniques:** none (phenomenon: validity evidence), PLS-SEM surveys, regression on LLM decisions, LLM test generation
- **Effect:** validity asserted via thresholds or argument
- **Because** (reader): conventional cutoffs and plausibility arguments stand in for tests
- **Unless:** none stated
- **Stated:** implied · **Interesting:** no — Trite: restates validity checks.
- **Papers (3; human, testing):** citywalk_enhancing_llm_based_c_unit, from_gains_to_strains_modeling_develop, once_upon_a_team_investigating_bias_in
- **Instances (6):** human/from_gains_to_strains_modeling_develop:72, human/from_gains_to_strains_modeling_develop:77, human/once_upon_a_team_investigating_bias_in:113, human/once_upon_a_team_investigating_bias_in:115, testing/citywalk_enhancing_llm_based_c_unit:132, testing/citywalk_enhancing_llm_based_c_unit:133
- **Stem:** none | none | validity

```
assert(validity(study, approach=none))
  because: loadings ≥ 0.70 ∧ vif < 3 | leakage(training-data) no-effect-on results (argued)
```

### NH-19

Agreement checks (LLM judges versus humans across prompts and contexts, focus groups versus statistical models, coder versus coder) are reported as holding or stable without a statistical test or reliability coefficient, with numbers deferred to replication packages or replaced by negotiated agreement.

- **When:** robustness and agreement side-analyses
- **Techniques:** none (phenomenon: agreement reporting), LLM-as-a-judge, focus groups, qualitative coding
- **Effect:** agreement asserted or null differences reported without test
- **Because** (reader): agreement analyses serve as secondary robustness checks and are summarised qualitatively
- **Unless:** none stated
- **Stated:** reader · **Interesting:** no — Trite: a reporting-practice prevalence observed in these papers.
- **Papers (3; ai4se, human):** group_versus_individual_review_requests, on_the_effectiveness_of_llm_as_a_jud, practitioner_views_on_mobile_app_access
- **Instances (5):** ai4se/on_the_effectiveness_of_llm_as_a_jud:62, ai4se/on_the_effectiveness_of_llm_as_a_jud:64, ai4se/on_the_effectiveness_of_llm_as_a_jud:96, human/group_versus_individual_review_requests:76, human/practitioner_views_on_mobile_app_access:24
- **Stem:** none | none | agreement

```
assert(agreement(judge, reference, approach=none) = stable)
  because: check(agreement) = secondary → report(agreement) = qualitative
```

### NH-20

Population-scale descriptors (popularity of dependents, reviewers per revision, dataset pairs per difficulty rating) are used to characterise samples or rule out confounds, and harder strata hold fewer samples because fewer people solve harder problems; such descriptors are sometimes asserted against their own medians.

- **When:** sample characterisation in human-activity datasets
- **Techniques:** human-and-practice (population descriptors), protestware dependents, code review requests, competitive programming correctness datasets
- **Effect:** counts/popularity reported; parity used to exclude a confound; count decreasing in difficulty
- **Because** (paper): solvers(problem) decreasing-in rating -> submissions(sampled) decreasing-in rating -> count(pairs) decreasing-in rating
- **Unless:** none stated
- **Stated:** implied · **Interesting:** no — Trite: descriptive population statistics.
- **Papers (3; formal, human):** developer_reactions_to_protestware_in_o, group_versus_individual_review_requests, hoareprompt_structural_reasoning_abo
- **Instances (3):** human/developer_reactions_to_protestware_in_o:111, human/group_versus_individual_review_requests:43, formal/hoareprompt_structural_reasoning_abo:208
- **Stem:** human-and-practice | population | scale

```
describe(size(population, approach=human-and-practice))
  because: solvers(problem) decreasing-in rating → submissions(sampled) decreasing-in rating → count(dataset, pairs) decreasing-in rating
```
