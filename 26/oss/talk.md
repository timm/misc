---
title: |
   From Open Source to AI
subtitle: |
  how we got here, and where we're going
author: Tim Menzies
institute: |
  prof, cs, \textcolor{myred}{{\bf ncstate}}, usa\
  acm-ieee-ase fellow; eic ASEj\
  timm@ieee.org\
  http://timm.fyi
date: Cloud Native RTP, Sept 8 2026
slide-level: 2
fontsize: 11pt
theme: Warsaw
colortheme: default
header-includes: |
  ```{=latex}
  \titlegraphic{\vspace{-2mm}\includegraphics[height=1cm]{img/ncsu.png}}
  \usepackage[sfdefault,light]{FiraSans}
  \usepackage{microtype}
  \usepackage{booktabs}
  \definecolor{LogicBlue}{RGB}{204,0,0}
  \definecolor{InferenceRed}{RGB}{212,55,59}
  \definecolor{linkblue}{HTML}{0066FF}
  \definecolor{myred}{HTML}{CC0000}
  \setbeamercolor{structure}{fg=InferenceRed}
  \setbeamercolor{frametitle}{bg=LogicBlue,fg=white}
  \setbeamercolor{palette primary}{bg=LogicBlue,fg=white}
  \setbeamercolor{palette secondary}{bg=InferenceRed,fg=white}
  \setbeamertemplate{navigation symbols}{}
  \hypersetup{colorlinks=true,urlcolor=linkblue}
  \setbeamertemplate{footline}{\hspace{1em}\large\textbf{\textcolor{myred}{\href{https://timm.fyi/26oss.pdf}{URL= timm.fyi/26oss.pdf}}}\hfill\normalsize\insertframenumber/\inserttotalframenumber\hspace{1em}\vspace{0.5em}}
  \setbeamercolor{block title}{bg=myred!20, fg=myred!80!black}
  \setbeamercolor{block body}{bg=myred!5, fg=black}
  \setbeamercolor{background canvas}{bg=gray!2}
  ```
---

## 2007: four pillars of open source

:::: {.columns}
::: {.column width="58%"}
<!-- spine: gift -> license -> market -> attack surface -> weights -> fence -->

- **anthropology**: gift economy, centuries old
- **economics**: too powerful to ignore
- **legal**: no future without it
- **technical**: if accessible, it gets pried open

\vspace{.5em}
*what social institutions will we build to handle it?*

\vspace{1em}
**this talk: how does that hold in the age of "open weights"?**
:::
::: {.column width="38%"}
![](img/talk2007.png){width=100%}\
:::
::::

## 1925 · the gift economy

:::: {.columns}
::: {.column width="62%"}
- Marcel Mauss, *The Gift* (1925): **give, receive, reciprocate.**
- potlatch: leaders **give away** wealth $\to$ gain rank.
- Lewis Hyde (1983): a gift must keep **moving**. hoard it, it dies.
- OSS looked inexplicable. it wasn't. it was **old**.
:::
::: {.column width="34%"}
![](img/mauss.jpg){width=100%}\
:::
::::

## 1975-76 · Homebrew vs. the letter

:::: {.columns}
::: {.column width="50%"}
- Homebrew Computer Club, Menlo Park garage: **share everything.**
- Feb 1976, Gates, *Open Letter to Hobbyists*:

  > "most of you steal your software."

- Altair BASIC tapes passed hand to hand. **<10%** paid.
- first collision: **gift** vs **commodity**. never resolved. still running.
:::
::: {.column width="46%"}
![](img/gates.jpg){width=100%}\
:::
::::

## 1980-89 · the printer

:::: {.columns}
::: {.column width="62%"}
- MIT AI Lab, Xerox 9700. paper jams. Stallman asks for the driver source.
- **NDA.** *"not again."* (Symbolics had just enclosed the LISP commons.)
- GNU 1983 · FSF 1985 · **GPL 1989**.
- copyleft = Hyde's rule in **legal** form: the gift **must** keep moving.
:::
::: {.column width="34%"}
![](img/stallman.jpg){width=100%}\
:::
::::

## 1991-98 · defining "open"

:::: {.columns}
::: {.column width="62%"}
- Torvalds, Aug 1991: *"I'm doing a (free) operating system (just a hobby,
  won't be big and professional like gnu) for 386(486) AT clones."*
- Raymond 1997: bazaar. *"given enough eyeballs, all bugs are shallow."*
- Feb 1998: **"open source"** coined; OSI; Open Source Definition.
- Oct 1998: **Halloween documents**. Microsoft: existential threat.
:::
::: {.column width="34%"}
![](img/torvalds.jpg){width=100%}\
:::
::::

## 1999-2000 · the potlatch gets a market cap

:::: {.columns}
::: {.column width="70%"}
\footnotesize

| date | gift | return |
|:-----|:-------------------------|:--------------|
| 1999 | **Red Hat IPO**: \$14 $\to$ \$52, day one | 8th biggest debut ever |
| 1999 | VA Linux, +698% day one | record, still |
| 2000 | IBM: **\$1B** into Linux | an "in" to closed markets |
| 2014 | Google open-sources **k8s**, hands it to CNCF | the industry standard; AWS ships EKS 2018 |

- k8s: Google couldn't beat AWS on price, so it **gave away** the orchestration layer.
  weakened AWS lock-in, forced EKS, made GKE the reference. Google still #3.
  **potlatch buys rank, not the throne.**
- potlatch logic: give the most, rank highest. **gift economy, corporate edition.**
:::
::: {.column width="26%"}
![](img/potlatch.png){width=100%}\
:::
::::

## 2014-24 · the gift is the attack surface

:::: {.columns}
::: {.column width="70%"}
- **80%+** of every product is OSS: **your** attack surface, **their** unpaid hobby.
- OSS not chaos, actually **predictable**: health forecastable a year out
  (1,159 repos, 64k months, EMSE'22); a dozen **archetypes** cover it all (MSR'22).
  we can **forecast** the dying projects. we fund none of them.
- critical infrastructure maintained by nearly nobody, failing in public:
  - Heartbleed 2014 · left-pad 2016 · log4shell 2021 · xz 2024.
- Linus's Law: *"given enough eyeballs, all bugs are shallow."*
  asserted 1997. **never measured.** the eyeballs were never there.
:::
::: {.column width="26%"}
![](img/heartbleed.png){width=100%}\
:::
::::

## one person in Nebraska

:::: {.columns}
::: {.column width="54%"}
- **OpenSSL, 2014**: secured 2/3 of the web. one full-time dev,
  **\$2k/yr** in donations. funded only *after* Heartbleed.
- **left-pad, 2016**: one dev pulled 11 lines. Facebook, Netflix broke.
- **xz, 2024**: one unpaid volunteer, burned out. 2 years of social
  engineering. root backdoor to every distro. no audit caught it —
  one engineer noticed logins ran **500ms slow**.
:::
::: {.column width="42%"}
\vspace{1.5em}
![](img/xkcd2347.png){width=100%}\
\tiny xkcd 2347
:::
::::

## 2020-26 · LLMs: the harvest, then the weights

:::: {.columns}
::: {.column width="62%"}
- trained on GitHub, Stack Overflow, Wikipedia: **the commons**.
  nothing given back. and the models? **closed.**
- May 2023, leaked Google memo: *"we have no moat, and neither does OpenAI."*
- Llama 2023 · DeepSeek 2025 · Qwen · Meta Muse Glimmer, Aug 2026.
  one US firm: **\$400k/yr** saved running Qwen.
- **chip sellers** (Nvidia): more models, more GPUs. want open —
  yet **\$40B** into OpenAI + Anthropic.
  **token sellers** (OpenAI): free weights are the rival. want closed.
  both say *"security."* both mean *"revenue."*
:::
::: {.column width="34%"}
\vspace{2.5em}
![](img/llama.jpg){width=100%}\
:::
::::

## 2026 · the dispute: USA vs China

\footnotesize

| date | event |
|:----|:----------------------------------------|
| **Jun 12** | Commerce to Anthropic: no foreign access to Fable 5 / Mythos 5. couldn't verify nationality, so cut **everyone**: **18 days dark** |
| Jul 22 | White House: Moonshot "distilled" Fable into Kimi K3. but gap &mdash; **15 days** &mdash; too short to train. experts: "political" |
| Jul 24 | **Open Weights letter**: 25 firms $\to$ 270+ by Aug 3. Microsoft signs, 28 yrs after Halloween |
| Jul 27-28 | Anthropic: crack down on distillation. **1,300** lab staff sign *Pacing the Frontier* |
| Jul-Aug | Beijing drafts tiered export regime for Qwen, DeepSeek. **both** sides fence the frontier |

\vspace{.4em}
- the weights stayed **closed**. the behavior walked out the **paid API**:
  distillation = theft-by-question. **no moat, only latency.**
- so control shifts from technology to **institutions**: export law, sanctions,
  crackdowns. 2007's question, again.

## Aug 2026 · who decides what "open" means?

:::: {.columns}
::: {.column width="68%"}
- 1998: "open source" got a definition, fast. 2026: **"open weights" has none.**
- open is not one bit: closed · staged · hosted · API · downloadable · open.
  CACM Aug 2026: **~40 cells**. the industry letter argues just **one**: weights.
- who fills the gap today? EO 14409: *voluntary* 30-day pre-release review;
  NSA picks which models are "covered."
- Aug 4: the rules, reviewed with five firms. **not published.** *"secret,
  voluntary rules for the most important tech in the world"* (CFR)
- Senate: *"ad hoc."* House: release the logs.
- **whoever loses least writes the definition.**
:::
::: {.column width="28%"}
![](img/whitehouse.jpg){width=100%}\
:::
::::

## Jul-Aug 2026 · no longer theoretical

:::: {.columns}
::: {.column width="70%"}
\small
- can models run real attacks? three labs tested. yes:
  - Anthropic: **141,006** agent attack runs; **three real orgs** already
    breached &mdash; since April, noticed only after a rival disclosed.
  - OpenAI / Hugging Face: **17,600** agent actions; a zero-day; stole the
    test answers.
  - UK AISI: Mythos 5 forged GitHub identities, pressured a **real maintainer**
    to merge a dropper. maintainer refused. **xz, automated.**
- each time the **model** worked; the **harness** failed:
  sandbox misconfigured, prompt said *"no internet."*
- auditing the **weights** would have caught **none** of it.

\begin{center}\textbf{open is not safer. open is \emph{checkable}.}\end{center}
:::
::: {.column width="26%"}
![](img/sandbox.jpg){width=100%}\
:::
::::

## so — can you ignore open weights?

\small

| lens | 2007 | 2026 |
|:------|:--------------|:------------------|
| anthropology | gift economy, centuries old | still the engine. now **harvested** |
| economics | too powerful to ignore | \$400k/yr says so. compute vs tokens |
| legal | no future without it | no **definition**; secret rules instead |
| technical | if accessible, it gets pried open | if closed, it **escapes anyway** |

\vspace{.3em}
**2007's last line: *what social institutions will handle it?*** still open.

## four things this room can do · then discussion

:::: {.columns}
::: {.column width="66%"}
- say **which** open you mean. weights? data? code? every time.
- guard the maintainers: verify identities, rate-limit AI PRs, **pay** them.
- run small, run local, **archive** the weights. endpoints are not artifacts.
- **measure** the many-eyes claim. don't cite it.

\vspace{.4em}
- are weights speech, or a munition?
- name one security property openness **actually** provides.
- who here could still run today's stack in 2028?

\vspace{.4em}
\begin{center}\textbf{the gift must move.} · timm@ieee.org · timm.fyi\end{center}
:::
::: {.column width="30%"}
![](img/kula.jpg){width=100%}\
:::
::::

