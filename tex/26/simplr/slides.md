---
title: |
  The Unreasonable Effectiveness\newline
  of Simplicity in AI
subtitle: (Why less is more, even in the age of LLMs)
author: Tim Menzies
institute: |
  prof, cs, \textbf{ncstate}, usa\newline
  acm-ieee-ase fellow; eic ASEj\newline
  timm@ieee.org\newline
  http://timm.fyi
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


## Australia: a Weird and Challenging Place
\begin{center}
\includegraphics[width=.4\textwidth]{plat3.png}
\end{center}

- **A Larrikin Culture:** loves knocking tossers off pedestals.
- **Weird & Wonderful:** Categories defined elsewhere are tested/broken here.
  - Heck, in Oz, even the water is a contrarian (swirls clockwise).
- **The Outback Test:** Ideas tamed elsewhere work in the wild?
- Oz's vastness demands rugged, simple, and efficient tech.
- **Embrace the Ozzieness:** Explore often the beaten track.



## The Prevailing Myth
- **The Narrative:** Conventional AI "failed" and was "repaired" by LLMs.
- **The Reality:** Pre-LLM AI was finding elegant shortcuts.
- We were discovering deep insights into human cognitive styles.
- These insights are being swept away in the urge to commercialize.
- Is "Bigger" always "Better," or just "More Expensive"?

## A Millennial Tradition of Simplicity
- **130 AD (Ptolemy):** Explain phenomena by the simplest hypothesis.
- **1300s (Occam):** Entities must not be multiplied beyond necessity.
- **1984 (JL Lemma):** Random projections preserve pairwise distances [8].
- **1997 (Feature Selection):** We can often ignore 80% of features [9].
- **2002 (Backdoors):** Setting a few variables cuts exponential time [10].
- **2025 (Compact AI):** Performance $\approx$ LLMs at lower cost [2].

\begin{center}
\includegraphics[width=.8\textwidth]{backdoors7.png}
\end{center}

## Why Simplification Studies are Rare


* In a survey of 229 SE papers on LLMs, only **5%** used baselines [11].
* Biased against simplicity [12]
* Without a "Simpler" baseline, we aren't doing Science.
* We must support alternatives for strategic, auditable tasks.

\begin{minipage}{0.52\textwidth}
\includegraphics[height=1.5in]{tiles10.png}
\end{minipage}\begin{minipage}{0.48\textwidth}
\includegraphics[height=1.5in]{lego10.png}
\end{minipage}

## Mapping AI to Human Cognition: RDR
- Paul Compton, **Ripple Down Rules (RDR):** Paradigm shift in Knowledge Acquisition.
- Avoids the "KA Bottleneck" by supporting **Repair in Context**.
- Experts justify why *this* case is different.
- Maintenance and acquisition become the same task [1].

\begin{minipage}{0.48\textwidth}
\includegraphics[width=\textwidth]{cases4.png}
\end{minipage}
\hfill
\begin{minipage}{0.48\textwidth}
\includegraphics[width=\textwidth]{time4.png}
\end{minipage}



## Better than the Masters: PIGS
- Timm: **AUSPIG Expert System:** Back-end for pig growth simulation.
- **The Result:** It outperformed the humans who wrote its rules.
- Achieved $\approx$ 10% improvement in profitability ($\$/m^2/day$).
- **Key point:** Humans provided rules; the system provided consistency.
- Fielded globally (USA, Holland, Australia) [7].

\begin{center}
\begin{minipage}{0.6\textwidth}
\includegraphics[width=.7\textwidth]{pige5.png}
\end{minipage}
\end{center}

## Case Study: NB.awk
- That moment when you realize just how easy AI can be...

\footnotesize
```awk
function train(i,c) {
  Total++; c=$NF; Classes[c]++
  for(i=1; i<NF; i++) {
    if($i=="?") continue
    Freq[c,i,$i]++; if(++Seen[i,$i]==1) Attr[i]++ }}

function classify(i,c,t,best,bestc) {
  best=-1e30; for(c in Classes) {
    t=log(Classes[c]/Total)
    for(i=1; i<NF; i++) {
      if($i=="?") continue
      t+=log((Freq[c,i,$i]+1)/(Classes[c]+Attr[i])) }
    if(t>best) { best=t; bestc=c }} return bestc }

```

\normalsize

## The Case for Compact AI

* Strategic tasks (auditable) require non-LLM tools [2, 4].
* Timm: **EZR**: **E**a**z**y Reasone**r**
* Stochastic learner exploiting software's sparsity.
* Achieves 90% of peak results with only **32 samples**.



\begin{center}
\includegraphics[width=.9\textwidth]{ai7.png}
\end{center}


## LLM vs. Compact AI: A Comparison

| Feature | Large Language Models | Compact AI (EZR/RDR) |
| --- | --- | --- |
| **Resource Req.** | Massive (H100s/GWs) | Minimal (Laptop/AWK) |
| **Reasoning** | Probabilistic/Opaque | Causal/Auditable |
| **Data Needed** | Terabytes | Dozens of labels [3] |
| **Maintenance** | Retraining ($$$) | Local Repair (RDR) |

## Conclusion: Simplicity is a Superpower

* Simplicity isn't just "easier"; it's robust and auditable.
* Don't prove absence of errors; **herd** toward goals [4].
* Let's stop building "Bigger" and start building "Smarter."
* **Next Step:** Compare your next LLM project against 200 lines of LUA.

## References

* **[1]** Richards, D. "Two decades of RDR research," *KER*, 24(2), 2009.
* **[2]** Menzies, T. "The Case for Compact AI," *Comm. ACM*, 2025.
* **[3]** Anon. "How Low Can You Go? The Data-Light SE Challenge," *FSE*, 2026.
* **[4]** Menzies, T. "From Verification to Herding," *VerfAI*, 2026.
* **[5]** Menzies, T. "The Case for Compact AI," *Comm. ACM*, 2025.
* **[6]** Menzies, T. "Unreasonable Effectiveness of SW Analytics," *IEEE SW*, 2018.
* **[7]** Menzies, T. "An Expert System for Raising Pigs," *UNSW*, 1990.
* **[8]** Johnson, W.B. & Lindenstrauss, J. "Extensions of Lipschitz maps," 1984.
* **[9]** Kohavi, R. & John, G. "Wrappers for feature subset selection," *AIJ*, 1997.
* **[10]** Williams, R., Selman, B. et al. "Backdoors in Hard SAT Instances," 2002.
* **[11]** Yang, Y. et al. "Survey on deep learning for SE," *ACM CSUR*, 54(10), 2022.
* **[12]** Adams, G.S. etl. “People systematically overlook subtractive changes,” Nature, 592(785), 2021.
