---
title: "The Mitochondria of the Mind"
subtitle: "(Why Compact AI is the Next Evolutionary Leap)"
author: "Tim Menzies"
institute: |
  prof, cs, **ncstate**, usa  
  acm-ieee-ase fellow; eic ASEj  
  timm@ieee.org  
  http://timm.fyi  
date: |
  March 3, 2026  
  \includegraphics[width=1.7cm]{qrcode.png}

slide-level: 2
fontsize: 9pt
theme: Warsaw
beamerthemeoptions:
  - footline=frame number

header-includes:
  - |
    \usepackage[sfdefault,light]{FiraSans}

    \definecolor{LogicBlue}{RGB}{204,0,0}
    \definecolor{InferenceRed}{RGB}{212,55,59}
    \definecolor{linkblue}{HTML}{0066FF}

    \setbeamercolor{structure}{fg=InferenceRed}
    \setbeamercolor{frametitle}{bg=LogicBlue,fg=white}
    \setbeamercolor{palette primary}{bg=LogicBlue,fg=white}
    \setbeamercolor{palette secondary}{bg=InferenceRed,fg=white}

    \setbeamertemplate{headline}{\vspace{0.4cm}}

    \setbeamertemplate{section in head/foot}{}
    \setbeamertemplate{subsection in head/foot}{}
    \setbeamertemplate{mini frames}{}
    \setbeamertemplate{navigation symbols}{}

    \setbeamertemplate{footline}{
      \hfill
      \usebeamercolor[fg]{page number in head/foot}
      \usebeamerfont{page number in head/foot}
      \insertframenumber\,/\,\inserttotalframenumber\kern1em\vbox{\vskip0pt}
    }
    \setbeamercolor{page number in head/foot}{fg=black}

    \AtBeginSection{}
    \AtBeginSubsection{}

    \setbeamersize{text margin left=0.45cm, text margin right=0.45cm}

    \hypersetup{colorlinks=true,urlcolor=linkblue}

    \usepackage{tikz}
    \usepackage{eso-pic}
    \AddToShipoutPictureFG{
      \ifnum\value{framenumber}>1
      \begin{tikzpicture}[remember picture,overlay]
      \node[anchor=north east,yshift=-0.05cm,xshift=-0.2cm]
      at (current page.north east)
      {\fcolorbox{black}{white}{\includegraphics[width=1.2cm]{qrcode.png}}};
      \end{tikzpicture}
      \fi
    }
---

## "AI" Means One Thing. But It Shouldn't.

When people say AI today, they mean generative LLMs. But most
real-world AI tasks are not — or not entirely — generative:

\begin{center}
\small
\begin{tabular}{p{4.2cm}|p{5.8cm}}
\textbf{Fully Generative} & \textbf{Fully or Partly Compact} \\
\hline
Open-ended dialog       & Configuration tuning \\
Code synthesis          & Defect prediction \\
Image/text generation   & Project health forecasting \\
Summarization           & System optimization \\
                        & Process simulation \\
                        & Feature model selection \\
                        & RAG: chunk retrieval$^{*}$ \\
\end{tabular}
\end{center}

\vspace{2mm}
$^{*}$ See later in this talk. RAG splits itno low-frequency Bayes (chunk
matchin) then LLM for the dialog generation

**Is the AI field solving the wrong problem for most use cases?**

## Four Questions 

\begin{center}
\large
\begin{tabular}{rll}
\textbf{RQ1} & Can \textit{all} AI be simplified?
  & \textcolor{InferenceRed}{\textbf{No}} \\[4pt]
\textbf{RQ2} & Can \textit{some} AI be simplified?
  & \textcolor{InferenceRed}{\textbf{Yes}} \\[4pt]
\textbf{RQ3} & \textit{Should} some AI be simplified?
  & \textcolor{InferenceRed}{\textbf{Yes}} \\[4pt]
\textbf{RQ4} & \textit{When} will AI be simplified?
  & \textcolor{InferenceRed}{\textbf{Soon}} \\
\end{tabular}
\end{center}

\vspace{4mm}
\begin{center}
Answered using 120+ SE search-based SE tasks: ~\\
\includegraphics[width=.65\textwidth]{moot_clusters.png} ~\\
http://github.com/timm/moot
\end{center}

## RQ1: Can \textit{All} AI Be Simplified? No.

* **Wolpert's No Free Lunch:** No optimizer/search  algorithm wins everywhere [7.8].
  - Once you can define it, we can define where ``it'' won't work.
* **Genuinely generative tasks** require LLMs: open-ended dialog,
  creative synthesis, novel code generation.
* **But:** even generative pipelines contain non-generative parts.

\vspace{2mm}
**RAG as a case study:**

\begin{center}
\small
\begin{tabular}{lll}
\textbf{Stage} & \textbf{Task} & \textbf{Method} \\
\hline
Chunk retrieval  & Match query to text & Low-freq.\ Bayes \\
Re-ranking       & Score candidates    & Sparse features  \\
Response gen.    & Produce dialog      & LLM (needed)     \\
\end{tabular}
\end{center}

\vspace{2mm}
* The expensive part is often the part you **don't** need LLMs for.
* Compact AI doesn't replace LLMs — it **origanizes** them. Agentic systems.

## RQ2: Can \textit{Some} AI Be Simplified? The Landscape

* "Rolling hills" model of gradient descent:  wrong?
* **PromiseTune:** Best performance clusters in tiny regions [3].
* Most of the space is desert; optimal "oases" are sharp spikes.
* **"How Low Can You Go?":** Spaces are isolated spikes [4].
  -  If you aren't on a spike, you are nowhere.

\begin{center}
\includegraphics[height=.8in]{hills.png}\hspace{10mm}%
\includegraphics[height=1.2in]{promisetune.png}
\includegraphics[height=1.3in]{bingo.png}
\end{center}

*Implication: gradient descent walks across valleys to find
peaks it could jump to. "Energy-blind. It walks when it
should teleport."*

## RQ2: MOOT Results (120+ tasks)  — How Good is Good Enough?

**Method**: Build regression tree from labels in  any $B$ rows

- Sort holdout set by tree predictions.
- Label top $C$ items in that sort.
- Return row $r$ with best labelt
- Score $r$ relative to pre-treated $\mu$ to $\min$ (reference optimal)
- $W(r)=100*(1 + (D(r)- \mu)/(\mu - \min)$

\begin{center}
\includegraphics[width=.25\textwidth]{bc.png}\hspace{2mm}
\includegraphics[width=.72\textwidth]{rands.png}
\end{center}

* **Left:** Relative results: $W(r)$ as a function of budget $B$ and
  checks $C$. 
  $W(r) = 55+0.4*  (B *C)$. More that 50 labels> 80% of optimum.
* **Right:** Absolute values:  120+ data sets sorted by $D$. Black is untreated
(before ($\mu$)). Even tiny budgets ($B{=}8$)
  rapidly find rows near best — most gains in first 10\%
  of data.
* Conclusion: the landscape is sparse enough that
  **small, cheap samples land near the optimum**.

## RQ2: Why? Software is Inherently Simple

* **The Funneling Effect:** Config spaces ($10^{40}$ options)
  collapse into a few critical paths.
* **Miller's Law:** Humans hold   7 $\pm$ 2 chunks in working memory.
  - Makes us wrote sparse code?
* **Sparsity is everywhere in SE**:
  * **Logic:** 6,750 variable problems solved by 12 "keys".
  * **Code:** 20\% of files contain 80\% of bugs.
  * **Runtime:** Big-data apps use $<50$ distinct execution paths.
  * **Design:** NASA missions converge on $<12$ \% of decisions.

\begin{center}
\includegraphics[height=.8in]{backdoors.png}\hspace{1mm}%
\includegraphics[height=1in]{bigfuzz.png}
\end{center}

## RQ2: The Mechanism — The Eukaryotic Leap

* Prokaryotes shared everything. Chaos. No complex life possible.
* Eukaryotes built a **Nuclear Envelope**: walled off their core.
* This let them house **Mitochondria**: a super-charged battery.
* More power per task $\rightarrow$ everything we call life followed.

\vspace{1mm}
**Compact AI is the same move:**

* **Model-Based Reasoning (MBR)** = our Nuclear Envelope.
  Builds a private world model; jumps to spikes instead of walking.
* **Reduce power-per-task** $\rightarrow$ AI deployable everywhere,
  not just in GPU data centers.

\begin{center}
\includegraphics[width=.38\textwidth]{pro.png}%
\includegraphics[width=.56\textwidth]{eur.png}
\end{center}

## RQ2: KLAS — Sparse Agents, Heavy Tools

* How do we build Agentic workflows without LLM overhead?
* **KLAS:** Agents = lightweight orchestrators (the Nucleus);
  tools = the Mitochondria that do the real work.
* Evidence from **SmartOracle** [5]: Agent-to-Agent traffic
  is minimal.
  * **Agent-to-Tool traffic dominates**
    (Terminal=405, Triage=390 calls).
* Keep the reasoning core sparse; let deterministic tools work.

\begin{center}
\includegraphics[width=.49\textwidth]{tic.png}
\includegraphics[width=.49\textwidth]{states.png}
\end{center}

Newell, 1986: *"Subgoals are generated whenever problem solving
cannot proceed until another problem space has performed some
subtask."*

## RQ2: EZR — 40 Samples to 80\% Optimal

* If spaces are sparse, we don't need heavy optimizers.
* **40 Samples $\rightarrow$ 80\% Optimal** [4].
* EZR incrementally maintains $\sqrt{N}$ best,
  $N{-}\sqrt{N}$ rest; labels most-best and least-rest.
* Minimal Bayesian learners + random probes $\approx$ SOTA.

\begin{center}
\includegraphics[width=.45\textwidth]{smac.png}
\includegraphics[width=.54\textwidth]{used.png}
\end{center}

* **3 minutes on a laptop** vs.\ 3 weeks on a GPU cluster.
* **100$\times$ faster. Better accuracy. No cloud required.**

## RQ3: \textit{Should} Some AI Be Simplified? Yes.

Five reasons this is urgent **now**, not eventually:

* **Energy:** Training and agentic runtime costs projected
  to rise **100$\times$** [1]. Compact AI runs on milliwatts.
* **Explanation:** You cannot audit what you cannot simplify.
  Regulators are asking; compact models answer.
* **Verification:** Safety-critical systems (aerospace, medical)
  cannot ship unverifiable black boxes.
* **Science:** Irreproducible AI is not science. Compact models
  run on any lab's hardware.
* **Sovereignty:** LLM dependence = dependence on a centralized
  industrial complex. Compact AI has no such landlord.

\begin{center}
\includegraphics[width=.55\textwidth]{monster.png}
\end{center}

## RQ4: \textit{When} Will AI Be Simplified? Soon.

In 1999, Microsoft told me they were visited by Intel every
six months. Intel's only question: *what can Microsoft do to
ensure the next chip generation has a market?* I was a
minimalist then. I didn't get the job.

Twenty-five years later the field spent hundreds of billions
building systems that are 52\% wrong [1]. The GPU bubble is
the same conversation, 1000$\times$ larger.

\vspace{2mm}
* **LLM costs rising 100$\times$** — not falling fast enough [1].
* **52\%** of ChatGPT answers contain incorrect info.
* **70\%** of AI-generated code is faulty.
* **25\%** of AI suggestions accepted by Google devs.

\vspace{2mm}
The mitochondrial shift wasn't gradual — it crossed a
threshold. We are approaching that threshold now.

\begin{center}
\includegraphics[height=1.2in]{llm_failures_chart.png}\hspace{4mm}
\includegraphics[height=1.2in]{agentic.png}
\end{center}

## Conclusion: Join the Next Evolutionary Leap

Mitochondria didn't optimize prokaryotes.
They made eukaryotes **possible**.

If compact AI reduces average power-per-task by 100--1000$\times$,
what becomes *conceivable* that isn't today?

* Verification of safety-critical systems — **at scale**
* Reproducible science — **on a laptop**
* Agentic systems that re-optimize — **in real time**
* AI sovereignty — **without a centralized landlord**

\vspace{2mm}
Simple ain't stupid. It's the next leap.

\begin{center}
\includegraphics[width=.38\textwidth]{wolfpack.png}
\end{center}

\begin{center}
\textbf{Join the Wolfpack.}
Build the nuclear envelopes for the next stage of AI life.
\end{center}

## References
\footnotesize

**[1]** Menzies, T. "The Case for Compact AI,"
*Comm. ACM*, 2025.  
**[2]** Chen, P., and Menzies, T. "MOOT: Multi-Objective
Optimization Tasks Repository," 2024.  
**[3]** Chen, P. "PromiseTune: Unveiling Causally Promising
and Explainable Configuration Tuning,"
*arXiv:2507.05995*, 2025.  
**[4]** Ganguly, K.K., and Menzies, T. "How Low Can You Go?
The Data-Light SE Challenge," *FSE*, 2026.  
**[5]** Srinivasan, S., et al. "SmartOracle - An Agentic
Approach," *arXiv:2601.15074*, 2026.  
**[6]** Crawford, J.M., and Baker, A.B. "Experimental Results
on the Application of Satisfiability Algorithms to Scheduling
Problems," *AAAI*, 1994.  
**[7]** Wolpert, David H., and William G. Macready. "No free lunch theorems for optimization." IEEE transactions on evolutionary computation 1.1 (2002): 67-82.      
**[8]** Wolpert, D. H., & Macready, W. G. (1995). No free lunch theorems for search (Vol. 10, No. 12, pp. 2756-2760). Technical Report SFI-TR-95-02-010, Santa Fe Institute.
