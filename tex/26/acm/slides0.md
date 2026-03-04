---
title: "A Meditation on Minimalism"
subtitle: "(with empirical results)"
author: "Tim Menzies"
institute: |
  prof, cs, **ncstate**, usa  
  acm-ieee-ase fellow; eic ASEj  
  timm@ieee.org  
  http://timm.fyi  
date: "March 3, 2026"

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
---

## The 1999 Question

In 1999, Microsoft was visited by Intel every six months.

Intel's only question:
\begin{center}
\large\textit{``What can Microsoft do to ensure the next chip generation has a market?''}
\end{center}


* I was a minimalist. I didn't get the job.
* Twenty-five years later, complexity rules, simplicity ignored.

## Milliwatts vs. Megawatts

When people say "AI" today, they mean generative LLMs. 
But most real-world AI tasks are configuration, prediction, optimization, etc etc

* **Rising Costs:** LLM training and agentic runtime costs are rising 100$\times$.
* **Poor Performance:**  52\% of ChatGPT answers contain incorrect info.
* **Energy:** We are building energy-hungry monsters, while Compact AI runs on milliwatts.
* **The Engineering Question:** If anyone can do it for a dollar, can you do it for a penny?

\begin{center}
\includegraphics[height=1.5in]{monster.png}
\end{center}


## The Secret of Software: Sparsity

Why can AI be smaller? Because human-created systems are inherently simple.

* **Miller's Law:** Humans can only hold $7\pm2$ chunks in working memory—we are \textit{forced} to write sparse code.
* **Code:** 20\% of files contain 80\% of the bugs.
* **Runtime:** Big-data apps use $<50$ distinct paths.
* **Design:** NASA missions converge on $<12$\% of decisions.

\vspace{2mm}
We don't need an AI that explores everything—just the few paths that actually matter.

\begin{center}
\includegraphics[height=.8in]{backdoors.png}\hspace{1mm}%
\includegraphics[height=1in]{bigfuzz.png}
\end{center}


## The "50 Samples" Rule

We studied this across 120+ software engineering optimization tasks.

* Most of the search space is an empty desert; optimal solutions are sharp spikes.
* Small, incredibly cheap samples land right near the absolute optimum.
* **The Result:** Taking just 50 samples gets us to 80\% of the optimal solution.

\begin{center}
\includegraphics[width=.70\textwidth]{rands.png}
\end{center}


## 3 Minutes vs. 3 Weeks

If spaces are sparse, we don't need heavy optimizers.

We built an incremental active learner (EZR) that simply maintains a small list of the "best" results and ignores the rest.

\begin{center}
\includegraphics[width=.54\textwidth]{used.png}
\end{center}

* **3 minutes on a laptop** vs. 3 weeks on a massive GPU cluster.
* **100$\times$ faster. Better accuracy. No cloud required.**


## The Bubble Bursts?

\begin{center}
\includegraphics[width=\textwidth]{simpler.png}
\end{center}

## The Bubble Burst? (more)

\begin{center}
\includegraphics[height=1.5in]{lego.png}\hspace{5mm}
\end{center}

* Inference costs are dropping, but \textit{agentic runtime} costs are exploding.
* We are building expensive systems that are still 52\% wrong.

The world will not stop needing AI, but it will suddenly need AI it can afford.

\vspace{5mm}
\begin{center}
\Large\textbf{When the bubble breaks, compact AI must already exist.}

\vspace{3mm}
\textcolor{InferenceRed}{\textbf{We are building it now.}}
\end{center}



