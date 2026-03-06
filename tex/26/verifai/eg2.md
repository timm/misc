---
title: "A Meditation on Minimalism"
subtitle: "(with empirical results)"
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
    \usepackage{fancyvrb}
    \usepackage{lmodern}
    \usepackage[sfdefault,light,lining]{FiraSans}

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
## Questions

\begin{tabular}{rll}
\textbf{RQ0} & \textbf{Should} AI be simplified?
  & \textcolor{InferenceRed}{\textbf{Yes}} \\[4pt]
\textbf{RQ1} & Can \textbf{all} AI be simplified?
  & \textcolor{InferenceRed}{\textbf{No}} \\[4pt]
\textbf{RQ2} & Can \textbf{some} AI be simplified?
  & \textcolor{InferenceRed}{\textbf{Yes}} \\[4pt]
\end{tabular}

\vspace{.5in}

“Agentic AI? Sure! All agents LLMs? \textbf{\color{red}Maybe not.\color{black}}.”

## This talk:  RQs using 120+ search-based SE problems


\begin{center}
\small Studied using 120+ SE tasks: {\bf \color{red}http://github.com/timm/moot\color{black}}

\vspace{5mm}

\includegraphics[width=\textwidth]{moot_clusters.png}
\end{center}


## Premise: The world wants more AI

How?

- Increase the resources available per task
- Decrease the resources required per task

\vspace{2mm}


\begin{center}
\includegraphics[height=1.5in]{lego.png}\hspace{5mm}
\includegraphics[height=1.5in]{eur.png}
\end{center}


## "AI" Means One Thing. But It Shouldn't.

When people say AI today, they mean generative LLMs. But most
real-world AI tasks are not --- or not entirely --- generative:

\begin{center}
\small
\begin{tabular}{p{4.2cm}|p{5.8cm}}
\textbf{Fully Generative} & \textbf{Fully or Partly Compact} \\
\hline
Open-ended dialog       & Configuration tuning \\
Code synthesis          & Defect prediction \\
Image/text generation   & Project health forecasting \\
Summarization           & System optimization \\
                        & Effort estimation \\
                        & Process simulation \\
                        & Feature model selection \\
                        & RAG: chunk retrieval (see later)\\
                        & and dozens of other tasks 
\end{tabular}
\end{center}

\vspace{2mm}

## RQ0: Should AI be simplified?

\begin{center}
\includegraphics[width=\textwidth]{simpler.png}
\end{center}

## RQ0: \textit{Should} Some AI Be Simplified? Yes.

Five reasons this is urgent **now**:

* **Energy:** LLM training + agentic runtime costs rising
  **100$\times$** [1]. Compact AI runs on milliwatts.
* **Explanation:** You cannot audit what you cannot
  simplify. Regulators are asking; compact models answer.
* **Verification:** Safety-critical systems (aerospace,
  medical) cannot ship unverifiable black boxes.
* **Science:** Irreproducible AI is not science. Compact
  models run on any lab's hardware.
* **Sovereignty:** LLM dependence = dependence on a
  centralized industrial complex.
  Compact AI has no such landlord.

\begin{center}
\includegraphics[width=.52\textwidth]{monster.png}
\end{center}



## RQ0: Long history of "Less"

- \textbf{The Engineering Question}
  - If anyone can do it for a dollar, \textbf{\color{red}can you do it for a penny?\color{black}}
- Ken Thompson: "One of my most productive days was throwing away 1,000 LOC."
- "Less" = insightful,  more reliable, less expensive 

\begin{center}
\begin{minipage}{2in}
\includegraphics[width=1.5in]{shannon1.png}
\end{minipage} \begin{minipage}{2.5in}
\includegraphics[width=1.8in]{shannon2.png}
\includegraphics[width=1.8in]{shannon3.png}
\end{minipage}
\end{center}

## R0: Dieter Rams: Less, but Better
\vspace{5mm}
\begin{center}
\begin{minipage}{1.6in}
\includegraphics[width=1.6in]{rams0.png}
\end{minipage}\hspace{2mm}\begin{minipage}{3in}
\includegraphics[width=2.8in]{rams1.png}
\includegraphics[width=2.8in]{rams2.png}
\end{minipage}
\end{center}

## R0: history of AI: Lots of "Less"

\small

- 130 A.D., Ptolemy: “We consider it a good principle to explain the phenomena by the simplest
hypothesis possible”.
- 1310 A.D: William of Occam: "Plurality should not be posited without necessity".
- 1902, PCA: reduce data to a few principal component
- 1960s, Narrows: guide search via a few key variables.
- 1974, Prototypes: speed up $𝑘$-NN by reduce rows to a few exemplars.
- 1984, JL lemma: random projection to $𝑘$ = $𝑂(\epsilon−2 \log n)$ dimensions can preserve pairwise distances to within some error (1 ± $\epsilon$).
- 1986, ATMS: only focus diagnosis on core assumptions.
- 1994, ISAMP: a few restarts can explore large problems spaces.
- 1996, Sparse coding: learn efficient, sparse representations from data which inspired dictionary learning and sparse autoencoders.
- 1997, Feature selection: ignore up to 80% of features.
- 2002, Backdoors: if we first set a few variables, that cuts exponential tine to polynomial.
- 2005, Semi-supervised learning: data can be approximated on a much lower-dimensional
manifold.
- 2009, Active learning: only use most informative rows.
- 2003–2021, SE “keys”: a few parameters govern many SE models.
- 2010+, Surrogates: first, build small models to label the rest of the data.
- 2020s, Distillation: compress large LLM models with little performance loss.

\normalsize

## R0: applications of "Less": Safety 

**Menzies' 4th Law:** For SE, best to throw away most data.

\begin{center}
\includegraphics[height=1.2in]{pad.png}\includegraphics[height=1.2in]{bore.png}

\vspace{1.8in}
\end{center}

## R0: applications of "Less": Safety and Privacy

**Menzies' 4th Law:** For SE, best to throw away most data.

\begin{center}
\includegraphics[height=1.2in]{pad.png}\includegraphics[height=1.2in]{bore.png}

\includegraphics[height=1.8in]{fayaloa.png}
\end{center}

## R0: Humans routinely ignore "Less"

- Survey of 229 SE papers on LLMs. 5% compared to simpler approaches.
  - Why? Cognitive Bias?

\begin{center}
\includegraphics[width=4in]{less.png}
\end{center}

## RQ1: Can \textit{All} AI Be Simplified? No.

* **Wolpert's No Free Lunch** [7,8]: No optimizer wins
  everywhere. Once you can define ``it'', we can define
  where ``it'' won't work.
* **Genuinely generative tasks** require LLMs: open-ended
  dialog, creative synthesis, novel code generation.
* **But:** even generative pipelines contain
  non-generative parts.

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
* The expensive part is often the part you \textbf{don't}
  need LLMs for.
* Compact AI doesn't replace LLMs --- it \textbf{organizes}
  them inside agentic systems.

## RQ2: Can \textit{Some} AI Be Simplified? The Landscape

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


## RQ2: Can be simpler?   --- A Testable Prediction

If search spaces are spikey (not rolling hills) then
gradient descent should show:

* **Long plateaus** as it traverses the empty valleys
* **Sudden bursts** of improvement as it hits a spike

\begin{center}
\includegraphics[width=.7\linewidth]{hillclimb.png}
\end{center}

This is not a critique of GD for generative tasks ---
it is a claim about \textbf{where GD is the wrong tool}.
Spikey spaces need teleportation, not walking.
Sparse samplers (EZR) teleport.

## RQ2: Can be simpler? Inherently, Software=Simple

* **The Funneling Effect:** Config spaces ($10^{40}$
  options) collapse into a few critical paths.
* **Miller's Law:** Humans hold $7\pm2$ chunks in working
  memory --- we are \textit{forced} to write sparse code.
* **Sparsity is everywhere in SE**:
  * **Logic:** 6,750-variable problems solved by
    12 ``keys''.
  * **Code:** 20\% of files contain 80\% of bugs.
  * **Runtime:** Big-data apps use $<50$ distinct paths.
  * **Design:** NASA missions converge on $<12$\%
    of decisions.

\begin{center}
\includegraphics[height=.8in]{backdoors.png}\hspace{1mm}%
\includegraphics[height=1in]{bigfuzz.png}
\end{center}

## RQ2: Can be simpler? MOOT Results (120+ tasks)

**Method**: Build regression tree from labels in any $B$ rows

* Sort holdout set by tree predictions.
* Label top $C$ items; return best row $r$.
* Score $r$ relative to pre-treated mean ($\mu$) and min ($\min$) of raw data.

$$
W(r)=100\times\!\left(1 + \frac{D(r)-\mu}{\mu - d_{\mathrm{min}}}\right)
$$

\begin{center}
\includegraphics[width=.25\textwidth]{bc.png}\hspace{2mm}
\includegraphics[width=.70\textwidth]{rands.png}
\end{center}

* **Left:** Empirically, $W \approx 55 + 0.4 \times B \times C$.    
  $W>50$ labels $\Rightarrow$ 80+\% of optimum. 
  More $B$ not useful.
* **Right:** 120+ datasets sorted by $d_{\mathrm{min}}$.
  Black = untreated ($\mu$).      
  Tiny $B{=}8$ finds near-best fast.
* \textbf{Small, cheap samples land near the optimum.}


## RQ2: Can be simpler? --- Incremental Active Learning

* If spaces are sparse, we don't need heavy optimizers.
* **50 Samples $\rightarrow$ 80\% Optimal** [4].
* EZR incrementally maintains $\sqrt{N}$ best,
  $N{-}\sqrt{N}$ rest; labels most-best and least-rest.
* Minimal , Bayesian active learning

\begin{center}
\includegraphics[width=.4\textwidth]{smac.png}
\includegraphics[width=.54\textwidth]{used.png}
\end{center}

* **3 minutes on a laptop** vs.\ 3 weeks on a GPU cluster.
* **100$\times$ faster. Better accuracy. No cloud required.**


## RQ2: Can be simpler? --- Text Mining
\begin{center}
\includegraphics[height=2.2in]{rag.png}
\end{center}

## Answers

\begin{tabular}{rll}
\textbf{RQ0} & \textbf{Should} AI be simplified?
  & \textcolor{InferenceRed}{\textbf{Yes}} \\[4pt]
\textbf{RQ1} & Can \textbf{all} AI be simplified?
  & \textcolor{InferenceRed}{\textbf{No}} \\[4pt]
\textbf{RQ2} & Can \textbf{some} AI be simplified?
  & \textcolor{InferenceRed}{\textbf{Yes}} \\[4pt]
\end{tabular}

\vspace{.5in}

“Agentic AI? Sure! All agents LLMs? \textbf{\color{red}Maybe not.\color{black}}.”


## My 1999 Story.

In 1999, Microsoft told me they were visited by Intel every
six months. Intel's only question:

\begin{center}
\large\textit{``What can Microsoft do to ensure the next
chip generation has a market?''}
\end{center}

I was a minimalist. I didn't get the job.

Twenty-five years later the field spent hundreds of billions
building systems that are 52\% wrong [1].
The GPU bubble is the same conversation, 1000$\times$ larger.

\vspace{2mm}
* **LLM costs rising 100$\times$** --- not falling fast
  enough [1].
* **52\%** of ChatGPT answers contain incorrect info [1].
* Inference costs dropping --- but \textit{agentic runtime}
  costs are the ones exploding.
* When the bubble breaks, compact AI must already exist.
  \textbf{We are building it now.}


## References

\begin{block}{}
\small
\textbf{[1]} Menzies, T. ``The Case for Compact AI,'' \textit{Comm. ACM}, 2025. \\
\textbf{[2]} Chen, P., and Menzies, T. ``MOOT: Multi-Objective Optimization Tasks Repository,'' 2024. \\
\textbf{[3]} Chen, P. ``PromiseTune,'' \textit{arXiv:2507.05995}, 2025. \\
\textbf{[4]} Ganguly, K.K., and Menzies, T. ``How Low Can You Go?'' \textit{FSE}, 2026. \\
\textbf{[5]} Srinivasan, S., et al. ``SmartOracle,'' \textit{arXiv:2601.15074}, 2026. \\
\textbf{[6]} Crawford, J.M., and Baker, A.B. ``Satisfiability Algorithms,'' \textit{AAAI}, 1994. \\
\textbf{[7]} Wolpert \& Macready. ``No free lunch theorems for optimization,'' \textit{IEEE Trans.\ Evolutionary Computation}, 2002. \\
\textbf{[8]} Wolpert \& Macready. ``No free lunch theorems for search,'' SFI-TR-95-02-010, 1995.
\end{block}
