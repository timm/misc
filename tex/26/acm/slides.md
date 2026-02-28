---
title: "The Mitochondria of the Mind"
subtitle: "(Why Compact AI is the Next Evolutionary Leap)"
author: "Tim Menzies"
institute: |
  prof, cs, **ncstate**, usa  
  acm-ieee-ase fellow; eic ASEj  
  timm@ieee.org  
  http://timm.fyi  
date: "February 2026"
slide-level: 2
fontsize: 10pt
theme: Warsaw
beamerthemeoptions:
  - footline=frame number
header-includes:
  - \usepackage[sfdefault,light]{FiraSans}
  - \definecolor{LogicBlue}{RGB}{204,0,0}
  - \definecolor{InferenceRed}{RGB}{212,55,59}
  - \definecolor{linkblue}{HTML}{0066FF}
  - \setbeamercolor{structure}{fg=InferenceRed}
  - \setbeamercolor{frametitle}{bg=LogicBlue,fg=white}
  - \setbeamercolor{palette primary}{bg=LogicBlue,fg=white}
  - \setbeamercolor{palette secondary}{bg=InferenceRed,fg=white}
  - \setbeamertemplate{navigation symbols}{}
  - \hypersetup{colorlinks=true,urlcolor=linkblue}
  - \usepackage[absolute,overlay]{textpos}
  - \addtobeamertemplate{frametitle}{}{\begin{textblock*}{1.5cm}(11.3cm,0.2cm)\includegraphics[width=1.2cm]{qrcode.png}\end{textblock*}}
---


## Prevailing Myths & The Reality of LLMs

* **Myth:** Conventional AI "failed" and was "repaired" by LLMs.
* **Reality:** LLMs are struggling with basic reliability:
  * **52%** of ChatGPT answers contain incorrect info.
  * **70%** of AI-generated code is faulty.
  * **25%** of AI suggestions are accepted by Google devs.
  * LLM costs are projected to rise **100x**.
* Is "Bigger" always "Better," or just "More Expensive"?

```
\begin{center}
\includegraphics[width=.48\textwidth]{agentic.png}
\includegraphics[width=.48\textwidth]{llm_failures_chart.png}
\end{center}
```

## The Real Landscape: The MOOT Repository

We studied 120+ datasets across the SE landscape [2]:
* **System Optimization:** Latency, throughput, cloud energy.
* **Product Line Engineering:** High-dimensional constraints.
* **Project Health:** Forecasting PRs, commits, issues.
* **Defect Mitigation:** Minimizing defect density.
* **Process & Cost:** Simulating Agile vs. Waterfall.

Our current "Bigger is Better" approach is an evolutionary dead end. We are at the end of the "Prokaryotic" era of AI.

```
\begin{center}
\includegraphics[width=.6\textwidth]{moot_clusters.png}
\end{center}
```

# PART 1: The Landscape is a Lie

## Heatmaps of Truth

* The "rolling hills" mental model of gradient descent is failing us.
* **PromiseTune results:** Performance clusters in tiny, non-smooth regions [3].
* Most of the search space is a desert; the optimal "oases" are sharp and narrow.
* **"How Low Can You Go?":** Spaces are isolated spikes [4].
* If you aren't on a spike, you are nowhere.

```
\begin{center}
\includegraphics[width=.7\textwidth]{promisetune_heatmap.png}
\end{center}
```

# PART 2: The Science of Simplicity

## Why Software is Inherently Simple

* **The Funneling Effect:** Software configuration spaces (e.g., $10^{40}$ options) usually collapse into a few critical paths.
* **Miller's Law:** Humans can only hold 7±2 chunks in working memory. We *must* write sparse code.
* **Sparsity is everywhere in SE**:
  * **Logic:** 6,750-variable problems solved by just 12 "keys".
  * **Code:** 20% of files contain 80% of bugs.
  * **Runtime:** Big-data apps use < 50 distinct execution paths.
  * **Design:** NASA space missions converge on < 12% of decisions.

```
\begin{center}
\includegraphics[width=.6\textwidth]{funneling_effect.png}
\end{center}
```
# PART 3: The Eukaryotic Leap

## The Primordial Soup to Eukaryotes

* In the primordial soup, Prokaryotes shared everything (horizontal hacking).
* The great leap happened when Eukaryotes built a **"Nuclear Envelope"**.
* They walled themselves off from change to protect their core instructions.
* This allowed them to house **Mitochondria**: a super-charged battery.
* **Model-Based Reasoning (MBR)** is our "Nuclear Envelope". It builds a private world model to glance across the landscape and jump.

```
\begin{center}
\includegraphics[width=.7\textwidth]{eukaryote_cell.png}
\end{center}
```

# PART 4: Knowledge-Level Agent Systems (KLAS)

## Mitochondria: Tools vs. Agents

* How do we build modern Agentic workflows without massive LLM overhead?
* **KLAS:** Agents act as lightweight orchestrators (the Nucleus); the heavy lifting is offloaded to tools (the Mitochondria).
* Evidence from **SmartOracle**: 
  * Agent-to-Agent traffic is minimal.
  * **Agent-to-Tool traffic dominates** (Terminal=405, Triage=390 calls).
* Keep the reasoning core sparse; let deterministic tools do the work.

```
\begin{center}
\includegraphics[width=.7\textwidth]{klas_architecture.png}
\end{center}
```

# PART 5: Minimalism in Action

## EZR — 32 Samples to 90% Optimal

* If spaces are sparse, we don't need heavy optimizers.
* **32 Samples $\rightarrow$ 90% Optimal** [4].
* EZR finds the "Best" and jumps there.
* Minimal Bayesian learners and random probes achieve near-SOTA with tiny data.
* Runs in 3 minutes on a laptop vs. 3 weeks on a GPU cluster.

```
\begin{center}
\includegraphics[width=.6\textwidth]{ezr_results.png}
\end{center}
```

# PART 6: The Challenge of Alien Code

## GenAI & The Elephant in the Room

* Because of Miller's Law (7±2), human code is inherently sparse and herdable.
* LLMs are not bound by human cognitive limits.
* Generative AI produces dense, high-dimensional, highly entangled dependencies.
* **The Threat:** "Alien Code" that humans cannot audit, maintain, or simplify.
* If we don't apply Compact AI constraints now, we lose control of the state space entirely.

```
\begin{center}
\includegraphics[width=.6\textwidth]{alien_code_monster.png}
\end{center}
```

## Conclusion: Build Walls, Not Bridges

* Stop building bigger bridges (brute-force Gradient Descent). 
* Start building better walls (Compact AI/MBR).
* Whether solving core SE problems or building the next generation of efficient Agentic systems, we need people building walls.
* **Join the Wolfpack.** Let's build the nuclear envelopes for the next stage of AI life.

```
\begin{center}
\includegraphics[width=.4\textwidth]{ncstate_wolfpack.png}
\end{center}
```

## References
\footnotesize

**[1]** Menzies, T. "The Case for Compact AI," *Comm. ACM*, 2025.  
**[2]** Chen, P., and Menzies, T. "MOOT: Multi-Objective Optimization Tasks Repository," 2024.  
**[3]** Chen, P. "PromiseTune: Unveiling Causally Promising and Explainable Configuration Tuning," *arXiv:2507.05995*, 2025.  
**[4]** Ganguly, K.K., and Menzies, T. "How Low Can You Go? The Data-Light SE Challenge," *FSE*, 2026.  
**[5]** Srinivasan, S., et al. "SmartOracle - An Agentic Approach," *arXiv:2601.15074*, 2026.  
**[6]** Crawford, J.M., and Baker, A.B. "Experimental Results on the Application of Satisfiability Algorithms to Scheduling Problems," *AAAI*, 1994.
