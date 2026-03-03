---
title: |
  The Mitochondria of the Mind\newline
subtitle: (Why Compact AI is the Next Evolutionary Leap)
author: Tim Menzies
institute: |
  prof, cs, \textbf{ncstate}, usa\newline
  acm-ieee-ase fellow; eic ASEj\newline
  timm@ieee.org\newline
  http://timm.fyi \newline\newline
date: February 2026
slide-level: 2
fontsize: 10pt
theme: Warsaw
beamerthemeoptions:
  - footline=frame number
header-includes:
  - '\usepackage[sfdefault,light]{FiraSans}'
  - '\definecolor{LogicBlue}{RGB}{204,0,0}'
  - '\definecolor{InferenceRed}{RGB}{212,55,59}'
  - '\definecolor{linkblue}{HTML}{0066FF}'
  - '\definecolor{myred}{HTML}{CC0000}'
  - '\setbeamercolor{structure}{fg=InferenceRed}'
  - '\setbeamercolor{frametitle}{bg=LogicBlue,fg=white}'
  - '\setbeamercolor{palette primary}{bg=LogicBlue,fg=white}'
  - '\setbeamercolor{palette secondary}{bg=InferenceRed,fg=white}'
  - '\setbeamertemplate{navigation symbols}{}'
  - '\hypersetup{colorlinks=true,urlcolor=linkblue}'
---

## The Illusion: AI $\neq$ GenAI

* When people hear 'AI', they picture Large Language Models.
* But LLMs are just one organism in a massive ecosystem.
* Real Software Engineering requires optimizing massive, complex spaces that you can't just "vibe" your way through [1]. 
* If we only focus on GenAI, we are ignoring the rest of the biosphere.

*insert image here*


## The Real Landscape: The MOOT Repository

We studied 120+ datasets across the SE landscape [2]:
* **System Optimization:** Latency, throughput, cloud energy.
* **Product Line Engineering:** High-dimensional constraints.
* **Project Health:** Forecasting PRs, commits, issues.
* **Defect Mitigation:** Minimizing defect density.
* **Process & Cost:** Simulating Agile vs. Waterfall.
* **Cross-Domain:** Finance, Medical, RL benchmarks.

*insert image here*


## The Wake-Up Call

* Most AI research optimizes based on 1 to 5 datasets.
* Looking at 120+ datasets reveals a hard truth:
* Our current "Bigger is Better" approach is an evolutionary dead end.
* We are at the end of the "Prokaryotic" era of AI.

*insert image here*
[Image contrasting N=5 narrow AI research focus versus N=120 broad landscape overview]

## PART 1: The Landscape is a Lie

* The "rolling hills" mental model is failing us.
* **PromiseTune results:** Performance clusters in tiny, non-smooth regions [3].
* Most of the search space is a desert; the optimal "oases" are sharp and narrow.

*insert image here*


## Spikes in the Dark

* **"How Low Can You Go?":** Spaces are isolated spikes [4].
* If you aren't on a spike, you are nowhere.
* Local repair (nudging) fails when the gap is a chasm.

*insert image here*


## Gradient Descent is an Expensive Bridge

* GD experiences "bursts" where it builds massive bridges over valleys.
* It's energy-blind. It walks when it should teleport.
* It relies on brute force to cross the chasm.

*insert image here*


## PART 2: The Eukaryotic Leap

* In the primordial soup, Prokaryotes shared everything (horizontal hacking).
* The great leap happened when Eukaryotes built a **"Nuclear Envelope"**.
* They walled themselves off from change to protect their core instructions.
* This allowed them to house **Mitochondria**: a super-charged battery.

*insert image here*


## Stop Walking, Start Jumping

* **Crawford (1994):** When solutions are distant, use Restarts [5].
* **Model-Based Reasoning (MBR)** is our "Nuclear Envelope".
* It builds a private world model to glance across the landscape and jump.
* This is the new Mitochondria: doing more with orders of magnitude less power.

*insert image here*


## PART 3: Energy as 'Time to Truth' (KLAS)

* Mitochondria: Tools vs. Agents [6].
* The **Agent** is the Nucleus (Compact, Walled-off Reasoning).
* The **Tools** are the Mitochondria (Metabolic Heavy Lifting).
* Keep the reasoning core sparse; let tools handle the brute computation.

*insert image here*


## The Sparsity Evidence

* Why can we afford to be "Small"?
* Software spaces funnel: 6,750 variables are often controlled by just 12 "master keys" [7].
* We don't need planetary-scale compute if we target the sparsity.

*insert image here*


## PART 4: EZR — Minimalism in Action

* **32 Samples $\rightarrow$ 90% Optimal** [4].
* EZR finds the "Best" and jumps there.
* Minimal Bayesian learners and random probes achieve near-SOTA with tiny data.
* Runs in 3 minutes on a laptop vs. 3 weeks on a cluster.

*insert image here*


## PART 5: The Elephant in the Room (GenAI)

* You might be thinking: This is great for configuration, but what about LLMs?
* **Prompt Spaces Collapse Too:** Massive prompt search spaces reduce to a minimized set of influential tokens [8].
* Millions of prompts exhibit redundant, near-equivalent behavior.
* GenAI isn't immune to the "Spiky/Sparse Landscape" rules.

*insert image here*


## Architecting the Future (Agentic Systems)

* Everyone wants to build Agentic systems.
* Choosing task breakdowns and tuning control parameters for agents is a massive optimization problem [9].
* These are exactly the kinds of problems represented in the MOOT repository.
* We need lightweight optimization methods to tune them, not brute force.

*insert image here*


## Conclusion: Build Walls, Not Bridges

* Stop building bigger bridges (brute-force GD). 
* Start building better walls (Compact AI/MBR).
* Whether solving core SE problems or building the next generation of efficient Agentic systems, we need people building walls.
* **Join the Wolfpack.** Let's build the nuclear envelopes for the next stage of AI life.

*insert image here*


## References
\footnotesize

**[1]** Menzies, T. "The Case for Compact AI," *Comm. ACM*, 2025.  
**[2]** Chen, P., and Menzies, T. "MOOT: Multi-Objective Optimization Tasks Repository," 2024.  
**[3]** Chen, P. "PromiseTune: Unveiling Causally Promising and Explainable Configuration Tuning," *arXiv:2507.05995*, 2025.  
**[4]** Ganguly, K.K., and Menzies, T. "How Low Can You Go? The Data-Light SE Challenge," *FSE*, 2026.  
**[5]** Crawford, J.M., and Baker, A.B. "Experimental Results on the Application of Satisfiability Algorithms to Scheduling Problems," *AAAI*, 1994.  
**[6]** Srinivasan, S., et al. "SmartOracle - An Agentic Approach," *arXiv:2601.15074*, 2026.  
**[7]** Menzies, T. "The Unreasonable Effectiveness of Simplicity in AI," *Presentation*, 2026.  
**[8]** Author, A. "Recent Work on Prompt Optimization and Search Space Collapse," *Literature Review*, 2025.  
**[9]** Menzies, T., et al. "Lightweight Methods for Agentic Systems Control Parameters," *Future Work / MOOT*, 2026.
