---
title: "From Verification to Herding"
subtitle: "(Exploiting Software's Sparsity of Influence)"
author: "Tim Menzies & Kishan Kumar Ganguly"
institute: |
  prof, cs, **ncstate**, usa  
  acm-ieee-ase fellow; eic ASEj  
  timm@ieee.org  
  http://timm.fyi  
date: |
  March 2026  
  \vspace{2mm}
  \includegraphics[width=1.7cm]{qrcode.png}
slide-level: 2
fontsize: 9pt
theme: Warsaw
beamerthemeoptions:
  - footline=frame number
header-includes:
  - |
    \usepackage{fancyvrb}
    \usepackage{lmodern}
    \usepackage[sfdefault,light,lining]{FiraSans}

    \definecolor{LogicBlue}{RGB}{204,0,0}
    \definecolor{InferenceRed}{RGB}{212,55,59}
    \definecolor{linkblue}{HTML}{0066FF}
    \definecolor{myred}{HTML}{CC0000}

    \setbeamercolor{structure}{fg=InferenceRed}
    \setbeamercolor{frametitle}{bg=LogicBlue,fg=white}
    \setbeamercolor{palette primary}{bg=LogicBlue,fg=white}
    \setbeamercolor{palette secondary}{bg=InferenceRed,fg=white}

    \setbeamertemplate{headline}{\vspace{0.4cm}}

    \setbeamertemplate{section in head/foot}{}
    \setbeamertemplate{subsection in head/foot}{}
    \setbeamertemplate{mini frames}{}
    \setbeamertemplate{navigation symbols}{}
    \hypersetup{colorlinks=true,urlcolor=linkblue}
---

## The Crisis in Verification

:::::::::::::: {.columns}
::: {.column width="50%"}

* Verification now takes \textcolor{myred}{\textbf{over half}} the project effort [1].
* Modern systems are a hyper-parameter nightmare (e.g., $10^{40}$ configurations).
* Software behavior is chaotic, discontinuous, and resists smooth gradient descent.
* **The Result:** We are drowning in a state space explosion.
:::
::: {.column width="50%"}
::: {.block title="The Modeling Trap"}
To use formal methods (Answer Set Programming, SAT solvers), you must first build a logical model of the software.

\vspace{2mm}
*But building a model of the code is often harder and more expensive than writing the code itself.*

\vspace{2mm}
*Q: how to herd, quickly?*

\vspace{2mm}
*A: optimization, data mining?*
:::
:::
::::::::::::::

## A Paradigm Shift: Verification to Herding

\vspace{2mm}
If we cannot prove the absence of all faults, we must actively steer the system toward "heaven". 

:::::::::::::: {.columns}
::: {.column width="45%"}
**Traditional Verification**
\vspace{1mm}

* Goal: Prove $\forall x \in X : Valid(S(x))$
* Find the one thing that breaks the system: $X \vdash \bot$
* **Complexity:** High (to say the least). Boiling the ocean?
:::
::: {.column width="55%"}
**Herding (Active Learning)**
\vspace{1mm}

* Goal: Find $x' \in X$ where $Utility(S(x')) > \tau$
* Maximization: $\arg\max_{x \in X} U(x)$
* **Complexity:** A lightweight search. Find a "good enough" configuration and stop.
:::
::::::::::::::

\vspace{4mm}

## Landscape Analysis

* ``Rolling hills'' model of gradient descent: wrong?
* **PromiseTune:** Best performance clusters in tiny
  regions [3].
* Most of the space is desert; optimal ``oases'' are
  sharp spikes.
* All the actions is in a few isolated spikes [4].
  - Gradient descent walks slowly when it could teleport.

\begin{center}
\includegraphics[height=.8in]{hills.png}\hspace{10mm}%
\includegraphics[height=1.1in]{promisetune.png}
\includegraphics[height=1.3in]{bingo.png}
\end{center}



## Why Herding Works: The Physics of Software

Why isn't finding the optimal configuration impossible in a space of $10^{40}$?

Because software spaces are **not flat**. They are inherently funneled and sparse.

::: {.block title="Sparsity of Influence"}
Not all variables are created equal. In massive state spaces, a tiny fraction of variables control the vast majority of the system's behavior. 
:::

\vspace{2mm}
We don't need to search the whole space. We just need to find the **master keys**.

## Sparsity at Every Layer of the Stack

Decades of empirical research confirm this funneling effect:

* \textcolor{myred}{\textbf{Logic:}} 6,750-variable SAT problems are often controlled by "backdoors" of just 12 variables [2].
* \textcolor{myred}{\textbf{Code:}} The Pareto principle holds—20% of the files contain 80% of the bugs.
* \textcolor{myred}{\textbf{Runtime:}} Big-data applications processing petabytes of data typically exercise fewer than 50 distinct execution paths [3].
* \textcolor{myred}{\textbf{Design:}} NASA deep-space mission optimizations converge on fewer than 12% of available decisions [4].

## The Root Cause: Miller's Law

Why is software so predictably sparse? Because humans wrote it.

:::::::::::::: {.columns}
::: {.column width="50%"}
* **Cognitive Limits:** Humans can only hold $7 \pm 2$ chunks of information in working memory.
* To manage complexity, human developers write highly structured, modular, and repetitive code.
* This limitation forces the state space to collapse into manageable, sparse pathways.
:::
::: {.column width="50%"}
::: {.block title="The Exploit"}
If human-written software is inherently sparse, then **lightweight Active Learning** is the optimal tool to navigate it.
:::
:::
::::::::::::::

## The Geometry of Herding

Instead of running expensive simulations (e.g., $10^4$ seconds per run), we use a nearly free geometric proxy ($10^{-4}$ seconds).

\vspace{2mm}
:::::::::::::: {.columns}
::: {.column width="55%"}
\footnotesize
```python
def nearer(row, best, rest):
    # Score = (dist to best) - (dist to rest)
    return dist(row, best) - dist(row, rest)

```

:::
::: {.column width="45%"}
**The Core Intuition:**
\vspace{2mm}

* `best`: The tiny cluster of the best configurations seen so far.
* `rest`: Everything else.
* **The Goal:** Scan the massive space of unseen options and find the one closest to `best` and furthest from `rest`.
:::
::::::::::::::

\vspace{4mm}

## The EZR Loop: Enforcing Sparsity

\vspace{2mm}
:::::::::::::: {.columns}
::: {.column width="55%"}
\footnotesize

```python
def ezr_loop(unseen, budget):
    seen = get_random(unseen, n=4)
    best, rest = split(seen)
    
    while len(seen) < budget:
        # 1. GUESS (Cheap)
        guess = max(unseen, 
            key=lambda r: nearer(r, best, rest))
        
        # 2. EVALUATE (Expensive)
        actual = ask_oracle(guess)
        unseen.remove(guess)
        
        # 3. LEARN & PRUNE
        seen.add(actual)
        best.add(actual)
        if len(best) > sqrt(len(seen)):
            rest.add(best.drop_worst()) 
            
    return seen.best()

```

:::
::: {.column width="45%"}
**The Minimalist Loop:**
\vspace{2mm}

1. **Sample:** Start with just 4 random probes.
2. **Guess:** Scan `unseen` using our cheap geometry, not the oracle.
3. **Evaluate:** Only invoke the expensive oracle for that *one* best guess.
4. **Prune:** \textcolor{myred}{\textbf{The Secret Sauce.}} Keep the `best` cluster tiny ($\sqrt{N}$). If it gets too big, throw the worst items into `rest`.
:::
::::::::::::::

## Rethinking the Search Space: EZR vs. The Rest

Why not just use standard Active Learning (Bayesian Optimization, SMAC, TPE)? Because they assume a fundamentally different physics of software.

\vspace{2mm}
:::::::::::::: {.columns}
::: {.column width="50%"}
**The "Heavy" Optimizers**
*(Gaussian Processes, SMAC, TPE)*

\vspace{1mm}
* **The Assumption:** "Marble in a Bowl." Assumes relatively smooth, continuous space (nearby points share similar values).
* **The Mechanism:** Builds expensive surrogate models (Random Forests, Covariance matrices) for complex acquisition functions:
  * Expected Improvement (EI)
  * Upper Confidence Bound (UCB)
  * Probability of Improvement (PI)
* **The Trap:** Computing the next guess becomes slower than just running the code.
:::
::: {.column width="50%"}
::: {.block title="The EZR Approach"}
**The "Compact" Optimizer**

\vspace{1mm}
* **The Assumption:** The "Desert of Spikes." Software spaces are discrete, chaotic, and heavily funneled by Miller's Law.
* **The Mechanism:** Rejects heavy probabilistic modeling. If spaces are just isolated spikes, just use \textcolor{myred}{\textbf{cheap geometry}} to jump to the nearest spike. 
* Don't map the whole ocean. Just sail toward the "Best" island and away from the "Rest."
:::
:::
::::::::::::::

\vspace{4mm}
:::::::::::::: {.columns}
::: {.column width="50%"}
\centering

:::
::: {.column width="50%"}
\centering

:::
::::::::::::::

## Stduies using 120+ search-based SE problems


\begin{center}
\small Studied using 120+ SE tasks: {\bf \color{red}http://github.com/timm/moot\color{black}}

\vspace{5mm}

\includegraphics[width=\textwidth]{moot_clusters.png}
\end{center}


## The Minimalist Flex: 32 Samples

Does it work? We tested this on **120+ datasets** in the MOOT repository (configuration, cloud tuning, process estimation).

::: {.block title="The Data-Light Challenge"}
Using simple Active Learning (like EZR), we achieve \textcolor{myred}{\textbf{90\% of the best reported results}} using only \textcolor{myred}{\textbf{two to three dozen samples}} [5].
:::

\vspace{2mm}
* **Speed:** 3 minutes on a laptop vs. weeks on a cluster.
* **Cost:** Fractions of a cent compared to planetary-scale compute.
* **Result:** We don't prove the system is perfect; we securely herd it to a near-optimal state.

## The Looming Challenge: "Alien Code"

:::::::::::::: {.columns}
::: {.column width="50%"}
Our methods work because human code obeys Miller's Law (7±2). 

\vspace{2mm}
**Large Language Models (LLMs) do not.**

\vspace{2mm}
LLMs face no cognitive limits. They can generate dense, high-dimensional, highly entangled dependencies. 
:::
::: {.column width="50%"}
::: {.block title="The Alien Code Warning"}
If AI generates code that violates human sparsity, it becomes **un-herdable** and impossible to audit.
:::
:::
::::::::::::::

\vspace{4mm}
*Open Research Question:* Do we need "Sparsity Constraints" as a fundamental safety requirement for Generative AI?

## Conclusion

\begin{center}
\Large\textit{``Stop trying to prove the absence of all bugs.\\Start herding the system toward heaven.''}
\end{center}

\vspace{4mm}

* **Verification** is falling victim to the modeling trap and state space explosion.
* **Herding** exploits the inherent sparsity of human-written code.
* **Compact AI** (Active Learning) can navigate these spaces with orders of magnitude less effort.
* **We must protect sparsity** from the challenge of AI-generated "Alien Code."

## References

::: block
\small
**[1]** Menzies, T., et al. "The Case for Compact AI," *Comm. ACM*, 2025.  
**[2]** Williams, C., et al. "Using deep structure to locate hard problems," *AAAI*, 1992.  
**[3]** Zhang, Q., et al. "BigFuzz: Efficient fuzz testing for data analytics," *ASE*, 2020.  
**[4]** Feather, M., et al. "Converging on the optimal attainment of requirements," *RE*, 2002.  
**[5]** Ganguly, K.K., and Menzies, T. "How Low Can You Go?" *FSE*, 2026.
:::

