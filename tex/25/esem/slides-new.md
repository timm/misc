---
title: |
   Industry can get any empirical\
   research it wants
subtitle: |
  (Publish open source data, and some example scripts.)\
  \
  \textcolor{myred}{\Large\textbf{PDF: timm.fyi/esem25.pdf}}
author: Tim Menzies
institute: |
  prof, cs, ncstate, usa\
  acm-ieee-ase fellow; eic ASEj\
  timm@ieee.org\
  http://timm.fyi
date: Nov'25
slide-level: 2
fontsize: 9pt
theme: Warsaw
colortheme: default
header-includes:
  - \titlegraphic{\vspace{-5mm}\includegraphics[height=3cm]{logo.png}}
  - \usepackage[sfdefault,light]{FiraSans}
  - \usepackage{microtype}
  - \definecolor{LogicBlue}{RGB}{204,0,0}
  - \definecolor{InferenceRed}{RGB}{212,55,59}
  - \definecolor{linkblue}{HTML}{0066FF}
  - \definecolor{myred}{HTML}{CC0000}
  - \setbeamercolor{structure}{fg=InferenceRed}
  - \setbeamercolor{frametitle}{bg=LogicBlue,fg=white}
  - \setbeamercolor{palette primary}{bg=LogicBlue,fg=white}
  - \setbeamercolor{palette secondary}{bg=InferenceRed,fg=white}
  - \setbeamertemplate{navigation symbols}{}
  - \setbeamertemplate{footline}{\hfill\textcolor{myred}{\textbf{timm.fyi/esem25.pdf}}\hfill\textcolor{linkblue}{\insertframenumber/\inserttotalframenumber}\hspace{1em}\vspace{0.5em}}
  - \setbeamercolor{block title}{bg=myred!20, fg=myred!80!black}
  - \setbeamercolor{block body}{bg=myred!5, fg=black}
  - \setbeamercolor{background canvas}{bg=gray!2}
---





## From Open Source Culture to Open Science

**Sources:** **[Men07]**, **[Men25]** 

* 2004 Portland open source ethos: "svn commit -m 'share stuff'"
* Culture: barefoot, rain-soaked, Ruby-on-Rails meetups, no ties
* Idea: reproducible SE — radical in 2004
* Lionel Briand (2006): "no one will give you data"
* PROMISE born from challenge to that skepticism
* Reproducible research in SE was rare
* Reuse and re-examination became standard practice
* Static code attributes became a shared community artifact

\vspace{1em}
\textcolor{gray}{\footnotesize [Graphic: Timeline of Portland 2004 to PROMISE; photos of early team]}

---

## Research Goal

The goal of this research is to aid **software engineering researchers and practitioners** to **reliably learn defect predictors from static code features** through **open data, reproducible scripts, and simple empirical methods**.

**Context:**

* Defects cluster in small regions of code **[Ham09]**
* QA budgets finite; targeted inspection improves yield
* Static code attributes debated (McCabe vs Halstead)
* Reproducibility in SE was nearly absent pre-2007
* Community lacked baseline, data, or shared methodology

**Approach:**

* Collect static attributes from NASA datasets
* Use pruning to test "metric X vs metric Y" debates
* Benchmark public domain learners
* Publish all data + scripts
* Encourage follow-on reproducible research

\vspace{1em}
\textcolor{gray}{\footnotesize [Graphic: Problem-solution diagram showing static metrics leading to predictors]}

---

## Birth of PROMISE and Early Success

**Vision (Part 1):** Annual conference for predictor models
**Vision (Part 2):** Repository of 100s of datasets (defects, effort, issues)

**Key Growth Factors:**

* Steering committee expansion: Boetticher, Weyuker, Ostrand, Ruhe
* Weekly dataset-harvesting sprints
* Movement to "Seacraft" data at Zenodo
* Early PROMISE papers re-examined old results (rare in MSR)

**Effect:**

* By 2018, 20 percent of TSE papers used PROMISE datasets
* Community finally had a shared empirical foundation

\vspace{1em}
\textcolor{gray}{\footnotesize [Graphic: Venn: MSR = data gathering; PROMISE = data reuse/analysis]}

---

## The 2007 Paper’s Core Contribution

**Research Question:**
Can static code attributes predict defect-prone modules?

**Why Important:**

* Bugs unevenly distributed across systems
* Inspection effectiveness rises exponentially with effort
* Predictors steer limited QA effort to the right code regions

**Counter-Arguments (1990s):**

* Specific metrics matter (McCabe vs Halstead)
* Static attributes are useless (Fenton, Shepperd, Ince)

**Innovation:**

* Used pruning to remove useless attributes
* Multi-attribute learners beat single-attribute models
* Shared all data + scripts for reproducibility

\vspace{1em}
\textcolor{gray}{\footnotesize [Graphic: Diagram of pruning pipeline + performance comparison]}

---

## Menzies's 1st Law: Different Projects Have Different Best Metrics

**"Different projects have different best metrics."**

**Evidence:**

* Examined 3 dozen metrics across 7 NASA datasets
* After pruning, each dataset used only 2-3 attributes
* No universal winning metric emerged
* Debates of 1990s contradicted by empirical data
* Halstead vs McCabe vs LOC: all dominant in some datasets

**Implications:**

* Stop debating individual metrics
* Gather all cheap metrics, prune later
* Reproducibility requires reporting pruning decisions

\vspace{1em}
\textcolor{gray}{\footnotesize [Graphic: Heatmap of attributes selected per dataset]}

---

## Menzies's 2nd Law: Combined Weak Signals Beat Strong Ones

**"Individually weak, combined they become a strong signal."**

**Evidence:**

* Static attributes individually weak (supported by critics)
* Combined via learners => large improvement over baselines
* Multi-attribute models outperform single-attribute ones
* Shows metrics contain complementary signals

**Impact:**

* Refuted earlier claims that static metrics are useless
* Boosted acceptance of predictors in practice

\vspace{1em}
\textcolor{gray}{\footnotesize [Graphic: Bar chart comparing single-attribute vs multi-attribute]}

---

## Industrial Adoption and Impact

**Industry Findings:**

* Wan et al. (395 practitioners): 90 percent open to adoption
* Telecom case study: 87 percent defects predicted; 72 percent fewer inspections
* Samsung (REMI): F1 = 0.68 for API-level prediction

**Cost-Effectiveness:**

* Static analysis tools vs statistical prediction
* Rahman et al.: no significant difference

**Why Widespread:**

* Easy to port to new languages
* Lightweight extraction of metrics
* Far easier to adapt than static analyzers

\vspace{1em}
\textcolor{gray}{\footnotesize [Graphic: Adoption funnel: research -> industry -> savings]}

---

## Menzies's 3rd Law: Turkish Toasters Predict Satellites

**"Turkish toasters can predict errors in deep space satellites."**

**Evidence:**

* Transfer learning research: Turkish white-goods models worked for NASA data
* Expected: complex transforms
* Actual: nearest-neighbor matching sufficed
* Suggests structural patterns shared across domains

**Interpretation:**

* Many distinctions we believe exist in software may be spurious
* SE data may live in much lower dimensions than assumed

\vspace{1em}
\textcolor{gray}{\footnotesize [Graphic: Toaster + satellite analogy; NN transfer diagram]}

---

## Menzies's 4th Law: Throw Most Data Away

**"For SE, the best thing to do with most data is to throw it away."**

**Evidence (stunning):**

* Github issues: ignore 80 percent of labels, still predict
* Effort estimation: ignore 91 percent
* Defect prediction: ignore 97 percent
* Data with thousands of rows modeled with a few dozen samples
* Manifold assumption + Johnson-Lindenstrauss explain reductions

**Meaning:**

* SE data highly redundant
* Random sampling + pruning often enough

\vspace{1em}
\textcolor{gray}{\footnotesize [Graphic: 97 percent grayed-out dataset visualization]}

---

## Menzies's 5th Law: Bigger Is Not Better

**"Bigger is not necessarily better."**

**Evidence:**

* Systematic review: only 5 percent of LLM SE papers compared to alternatives
* Tree-based models still outperform deep learning on tabular data
* Classic PROMISE-style learners often superior
* Empirical results easily reversed via hyperparameter tuning

**Conclusion:**

* Simpler methods often win
* Evaluate LLMs fairly against baselines

\vspace{1em}
\textcolor{gray}{\footnotesize [Graphic: Complexity vs performance curve]}

---

## Menzies's 6th Law: Data Quality Matters Less Than Expected

**"Data quality matters less than you think."**

**Evidence:**

* PROMISE datasets contained repeated rows, illegal values
* Injected increasing error into data
* Performance curve remained flat
* Predictors resilient to quality degradation

**Interpretation:**

* For prediction tasks, signals robust to noise
* But not a license for sloppy data collection

\vspace{1em}
\textcolor{gray}{\footnotesize [Graphic: Flat performance curve despite noise injection]}

---

## Menzies's 7th Law: Bad Learners Make Good Conclusions

**"Bad learners can make good conclusions."**

**Evidence:**

* Optimization work: CART trees predicted poorly but ranked well
* Ranking > absolute accuracy for many tasks
* Supports idea of using weak signals to prune search space

**Implication:**

* Predictive accuracy may be wrong metric
* Ranking-based evaluation more important in SE tasks

\vspace{1em}
\textcolor{gray}{\footnotesize [Graphic: Bad predictor but good ordering diagram]}

---

## Menzies's 8th Law: Science Has Mud on the Lens

**"Science has mud on the lens."**

**Evidence:**

* Hyperparameter tuning can reverse published results
* Many conclusions brittle
* A grad student with a GPU can overturn yesterday’s TSE paper
* Need stable, robust empirical foundations

**Challenge:**

* Identify which conclusions are stable
* Bayesian methods and effect sizes critical

\vspace{1em}
\textcolor{gray}{\footnotesize [Graphic: Muddy camera lens metaphor]}

---

## Menzies's 9th Law: Many Hard Problems, Aren't

**"Many hard SE problems, aren't."**

**Evidence:**

* Cohen: benchmark sophisticated vs simpler strawmen
* Simpler methods repeatedly found competitive
* Author often switched to simpler method one year later
* But: exceptions remain (safety-critical, generative tasks)

**Question to SE:**
"Have we really checked what is simple vs complex?"

\vspace{1em}
\textcolor{gray}{\footnotesize [Graphic: Simple method beating complex method]}

---

## From Success to Stagnation

**Four Phases:**

1. "Data? Good luck with that!"
2. "Maybe not completely useless."
3. "This is the gold standard now."
4. "A graveyard of progress."

**Problem:**

* Community overused datasets from 1981-2010
* Old datasets stifled creativity
* ASE now desk-rejects papers using PROMISE-2005-era data

**Modern Data Options:**

* 1100+ recent Github repos
* CommitGuru extraction
* MSR shift to raw data

\vspace{1em}
\textcolor{gray}{\footnotesize [Graphic: Cycle diagram ending in graveyard]}

---

## Future Directions

**PROMISE Revival:**

* Accept higher-quality datasets
* Encourage new data rather than reuse
* Evaluate quality of existing datasets
* Support curation over raw harvesting

**Cautions (Herbold):**

* MSR data often raw and noisy
* Little validation
* On-the-fly scraping can be harmful
* Newer data is not always better

\vspace{1em}
\textcolor{gray}{\footnotesize [Graphic: PROMISE vs MSR data quality spectrum]}

---

## What’s Hot Now

**Modern Research Themes:**

* DeepLineDP (line-level deep learning)
* Interpretability methods growing
* Multi-objective optimization replacing binary classification
* Hyperparameter search (SA, MaxWalkSat, GA)
* Minimal data / active learning / surrogate models
* Semi-supervised learning
* Landscape analysis of configuration spaces

\vspace{1em}
\textcolor{gray}{\footnotesize [Graphic: Evolution timeline 2007-2025]}

---

## Key Lessons

**From the Retrospective:**

1. Open science works: data + scripts = community
2. Simple methods often outperform complex ones
3. Static attributes are useful when combined
4. Transfer learning easier than assumed
5. Data redundancy is huge
6. Evaluation protocols must evolve
7. Large datasets not always better
8. Ranking often more important than accuracy
9. Many hard problems are not actually hard

\vspace{1em}
\textcolor{gray}{\footnotesize [Graphic: Checklist of 9 laws]}

---

## Challenge to the Community

**Critical Questions:**

* Are we evaluating tasks with the right metrics?
* Are we using enough modern data?
* Are simpler methods being ignored?
* Are our conclusions stable or brittle?
* How much can be done with minimal data?

**Call-to-Action:**
Adopt PROMISE-style "do-it-then-do-it-again" research.

\vspace{1em}
\textcolor{gray}{\footnotesize [Graphic: Question mark over SE workflow]}

---

## Download the Slides

\begin{center}
\Huge\textbf{timm.fyi/esem25.pdf}

\vspace{2em}

\Large (References clickable in PDF)
\end{center}

