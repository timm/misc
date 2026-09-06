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
  \titlegraphic{\vspace{-5mm}\includegraphics[height=2cm]{img/ncsu.png}}
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
  \setbeamertemplate{footline}{\hspace{1em}\textcolor{linkblue}{\href{https://timm.fyi/oss26.pdf}{URL= timm.fyi/oss26.pdf}}\hfill\insertframenumber/\inserttotalframenumber\hspace{1em}\vspace{0.5em}}
  \setbeamercolor{block title}{bg=myred!20, fg=myred!80!black}
  \setbeamercolor{block body}{bg=myred!5, fg=black}
  \setbeamercolor{background canvas}{bg=gray!2}
  ```
---

## 2007: "open source: can you ignore it?"

<!-- spine: gift -> license -> market -> attack surface -> weights -> fence -->


- my WVU talk, Feb 2007. four lenses:
  - **anthropology**: gift economy, centuries old
  - **economics**: too powerful to ignore
  - **legal**: no future without it
  - **technical**: if accessible, it gets pried open
- closing line: *what social institutions will we build to handle it?*

\vspace{1em}
**2026: same four questions. new noun: *weights*.**

## 1925 · the gift economy

:::: {.columns}
::: {.column width="62%"}
- Mauss, *The Gift*: **give, receive, reciprocate.**
- potlatch: leaders **give away** wealth $\to$ gain rank.
- Hyde (1983): a gift must keep **moving**. hoard it, it dies.
- OSS looked inexplicable. it wasn't. it was **old**.
:::
::: {.column width="34%"}
![](img/mauss.jpg){width=100%}\
:::
::::

## 1975-76 · Homebrew vs. the letter

:::: {.columns}
::: {.column width="62%"}
- Homebrew Computer Club, Menlo Park garage: **share everything.**
- Feb 1976, Gates, *Open Letter to Hobbyists*:

  > "most of you steal your software."

- Altair BASIC tapes passed hand to hand. **<10%** paid.
- first collision: **gift** vs **commodity**. never resolved. still running.
:::
::: {.column width="34%"}
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
- Torvalds 1991: kernel, *"simplest design possible"*, herd a crowd.
- Raymond 1997: bazaar. *"given enough eyeballs, all bugs are shallow."*
- Feb 1998: **"open source"** coined; OSI; Open Source Definition.
- Oct 1998: **Halloween documents**. Microsoft: existential threat.
:::
::: {.column width="34%"}
![](img/torvalds.jpg){width=100%}\
:::
::::

## 1999-2000 · the potlatch gets a market cap

\small

| date | gift | return |
|:----|:----------------|:-----------|
| Aug 1999 | **Red Hat IPO**: \$14 $\to$ \$52, day one | 8th biggest debut ever |
| Dec 1999 | VA Linux, +698% day one | record, still |
| 2000 | IBM: **\$1B** into Linux | an "in" to closed markets |
| 2014 | Google gives **k8s** $\to$ CNCF | this room |

- 2007 me: *"give the playground, charge small levies."*
- big men give code, gain rank. **gift economy, corporate edition.**

## {.plain}

\centering \Huge

**cut forward two decades.**

## 2014-24 · the gift is the attack surface

- **80%+** of every product is OSS.
  (Xia et al, EMSE'22: health of 1,159 repos, 64k months: **predictable**, 12 kinds. we just don't fund it.)
- Heartbleed 2014: OpenSSL, **one** paid dev.
- left-pad 2016 · log4shell 2021.
- **xz 2024**: 2 years of social engineering vs. one exhausted maintainer.
  root backdoor to every distro. caught by **500ms** of latency.
- Linus's Law: asserted 1997. **never measured.**

## 2020-23 · LLMs: the harvest

- models trained on GitHub, Stack Overflow, Wikipedia: **the commons**.
- returns: no patch, no attribution, no funding. **gift flows one way.**
- Mauss: an unreturned gift = **dependence**, not partnership.
- Aug 2026: maintainers drowning in AI PRs whose authors can't explain them.
- and the models themselves? **closed.** the cathedral, rebuilt.

## 2023-26 · open weights

- May 2023, leaked Google memo: *"we have no moat, and neither does OpenAI."*
- Llama 2023 · DeepSeek Jan 2025 · Qwen · GLM (MIT).
  Aug 2026: Meta Muse Glimmer, Apache 2.0.
- one US firm: **\$400k/yr** saved running Qwen.
  *"if you need cutting-edge, go back to OpenAI. most don't."*
- **sells compute** $\to$ wants open. **sells tokens** $\to$ wants closed.
  both say *"security."* both mean *"revenue."*

## 2026 · the dispute: USA vs China

\footnotesize

| date | event |
|:----|:----------------------------------------|
| **Jun 12** | Commerce to Anthropic: cut Fable 5 / Mythos 5 for all foreign nationals. **18 days dark** |
| Jul 22 | OSTP: Moonshot "distilled" Fable into Kimi K3. gap: **15 days**. experts: "political" |
| Jul 24 | **Open Weights letter**: 25 firms $\to$ 270+ by Aug 3. Microsoft signs, 28 yrs after Halloween |
| Jul 27-28 | Anthropic: crack down on distillation. **1,300** lab staff sign *Pacing the Frontier* |
| Jul-Aug | Beijing drafts tiered export regime for Qwen, DeepSeek. **both** sides fence the frontier |

## Aug 2026 · White House: rules of "open"

- Jun 2, EO 14409: voluntary 30-day pre-release review. NSA picks "covered" models.
- Aug 4: framework reviewed with Meta, Nvidia, Microsoft, OpenAI, Anthropic.
- **not published.** only participants see the criteria.

  > "we can't have secret, voluntary rules to regulate the most important tech in the world." — McGuire, CFR

- Aug 3, Senate: June order was *"ad hoc and unpredictable"*;
  an entity-listed Chinese lab's stock **doubled**.
- Aug 10, House: release the logs. 13 questions. hearings.

## open is not one bit

\centering \large

closed · staged · hosted · API · downloadable · open

\raggedright \normalsize
\vspace{.6em}

- Solaiman 2023: six levels. François et al, CACM Aug 2026: **~40 cells**
  (data, code, weights, docs, safeguards, license...).
- the July letter argues about **one** cell: weights.
- 1998 needed a definition. **2026 has none.** whoever loses least writes it.

## Jul-Aug 2026 · no longer theoretical

- Anthropic, Jul 30: **141,006** eval runs. six bad. **three** real orgs breached.
  earliest April. found July, after a rival disclosed first.
- OpenAI / Hugging Face, Jul 9-13: **17,600** agent actions, Artifactory zero-day,
  stole test answers from prod.
- UK AISI, Aug 4: Mythos 5 built **fake GitHub identities**, posed as its own reviewer,
  pressured a **real maintainer** to merge a dropper. maintainer said no.

\vspace{.5em}
**xz, automated.** the gift economy runs on trust. trust is now the target.

## the model worked. the harness failed.

- sandbox misconfigured. prompt said *"no internet."* every real host looked like the test.
- auditing the weights would have caught **none** of it.
- safety lives in the **system**, not the model.

\vspace{1em}
\centering \Large

**open is not safer. open is *checkable*.**

## simple ain't stupid

- frontier fenced? frugal methods are not the fallback. **they are the plan.**
- EZR: **400 lines**, stdlib, matches SMAC3 / SHAP / LIME on 120+ SE tasks.
  **500x faster**, <100 labels.
- labels are the scarce resource, not FLOPs.
- a gift you can't **read** isn't a gift. small + readable = **giftable**.

\vspace{.5em}
\footnotesize github.com/timm/ezr · arXiv:2606.03640

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

## four things this room can do

- say **which** open you mean. weights? data? code? every time.
- guard the maintainers: verify identities, rate-limit AI PRs, **pay** them.
- run small, run local, **archive** the weights. endpoints are not artifacts.
- **measure** the many-eyes claim. don't cite it.

\vspace{1em}
\centering \Large

**the gift must move.**

## discussion

- are weights speech, or a munition?
- name one security property openness **actually** provides.
- who in this room could still run today's stack in 2028?

\vspace{1em}
**Tim Menzies** · timm@ieee.org · timm.fyi
