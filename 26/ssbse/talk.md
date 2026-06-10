---
marp: true
theme: default
paginate: true
style: |
  section.invert       { background: #CC0000; color: #ffffff; }
  section.invert h1,
  section.invert h2,
  section.invert h3,
  section.invert strong { color: #ffffff; }
  section.lead.invert  { background: #CC0000; }
  section strong       { color: #CC0000; }
  section.invert blockquote { color: #ffffff; border-color: #ffffff; }
  section blockquote   { border-left: 6px solid #CC0000; padding-left: .6em; }
  section.lead h1      { color: #CC0000; }
  section img[alt~="portrait"] { border: 4px solid #CC0000; }
---

<!-- _class: lead -->
<!-- _paginate: false -->

# <!-- fit --> can we reason about the world?

### a short history of theory — and where scale runs out

**Tim Menzies**    
Computer Science, NC State, USA   
<timm@ieee.org>   
<http://timm.fyi>    
July 2026 

<!-- spine: confidence -> doubt -> computation -> frugal -->

---

![bg right:50%](https://github.com/KKGanguly.png)
![bg right:50%](https://amiiralii.github.io/pic.png)
![bg right:50%](https://srina1h.github.io/images/profile.jpg)

## credits

- **Kishan Ganguly** &middot; <https://github.com/KKGanguly>
  *BINGO* (FSE).
- **Amirali Rayegan** &middot; <https://amiiralii.github.io/>
  *simpler explanation* (JSS).
- **Srinath Srinivasan** &middot; <https://srina1h.github.io/>
  *EZR.py*.

---

<!-- _class: lead -->

## the bet

> the world is **legible**
> because it is **mathematical**.

everything after is footnotes.

---

![bg right:35%](https://upload.wikimedia.org/wikipedia/commons/thumb/f/fc/Galileo_Galilei_%281564-1642%29_RMG_BHC2700.tiff/lossy-page1-330px-Galileo_Galilei_%281564-1642%29_RMG_BHC2700.tiff.jpg)

## Galileo

> the book of nature is written in mathematics.

- nature is **legible**, not capricious.

---

![bg right:35%](https://upload.wikimedia.org/wikipedia/commons/thumb/f/f7/Portrait_of_Sir_Isaac_Newton%2C_1689_%28brightened%29.jpg/330px-Portrait_of_Sir_Isaac_Newton%2C_1689_%28brightened%29.jpg)

## Newton — unification I

- heaven + earth -> **one** law of gravity.
- same physics here and on Jupiter.
- Kelvin-Helmholtz: clouds in our sky **and** Jupiter's bands.

---

![bg right:35%](https://upload.wikimedia.org/wikipedia/commons/thumb/b/b0/James-Clerk-Maxwell-1831-1879.jpg/330px-James-Clerk-Maxwell-1831-1879.jpg)

## Maxwell — unification II

- electricity + magnetism + light -> **one** field.
- predicted **c** from two bench constants.
- purest *"we can reason about the world"* moment.
- continues: electroweak -> Standard Model -> (gravity: open).

---

![bg right:35%](https://upload.wikimedia.org/wikipedia/commons/thumb/b/b5/Emmy_Noether_%283x4_cropped%29.jpg/330px-Emmy_Noether_%283x4_cropped%29.jpg)

## Noether — why laws exist

- 1918: every **symmetry** -> a **conservation law**.
- time-symmetric -> energy conserved.
- explains *why* physics is legible at all.
- the deepest answer Galileo's bet ever got.

---

![bg right:35%](https://upload.wikimedia.org/wikipedia/commons/3/39/Laplace%2C_Pierre-Simon%2C_marquis_de.jpg)

## Laplace's demon

- know every position + momentum -> predict **everything**.
- determinism as the dream of total knowledge.
- Laplace sits on **both** sides of this story.

---

![bg right:35%](https://upload.wikimedia.org/wikipedia/commons/thumb/a/ad/Boltzmann2.jpg/330px-Boltzmann2.jpg)

## the demon retreats

- Maxwell + Boltzmann (1860s-70s): statistical mechanics.
- stop tracking every particle. reason about **ensembles**.
- temperature = ignorance, managed well.
- physics **chooses** statistics over the demon.

---

![bg right:50%](https://upload.wikimedia.org/wikipedia/commons/thumb/9/98/Blaise_Pascal_Versailles.JPG/330px-Blaise_Pascal_Versailles.JPG)
![bg right:50%](https://upload.wikimedia.org/wikipedia/commons/f/f3/Pierre_de_Fermat.jpg)
![bg right:50%](https://upload.wikimedia.org/wikipedia/commons/d/d4/Thomas_Bayes.gif)

## uncertainty gets a calculus

- Pascal-Fermat (1654).
- Bayes / Laplace: **learning from evidence**.
- ancestor of ML.

---

![bg right:40%](https://upload.wikimedia.org/wikipedia/commons/thumb/a/ab/Florence_Nightingale_%28H_Hering_NPG_x82368%29.jpg/330px-Florence_Nightingale_%28H_Hering_NPG_x82368%29.jpg)

## Nightingale — stats meets policy

- Crimea 1858: **coxcomb** diagrams.
- showed disease, not battle, killed soldiers.
- first to **rule by evidence**, in public.
- statistics as instrument of state, not theory.

---

![bg right:50%](https://upload.wikimedia.org/wikipedia/commons/thumb/f/f4/PSM_V82_D416_Henri_Poincare.png/330px-PSM_V82_D416_Henri_Poincare.png)
![bg right:50%](https://upload.wikimedia.org/wikipedia/commons/4/42/Kurt_g%C3%B6del.jpg)
![bg right:50%](https://upload.wikimedia.org/wikipedia/commons/thumb/c/ce/Alan_turing_header.jpg/330px-Alan_turing_header.jpg)

## the cracks

- Poincare: 3 bodies break determinism.
- quantum: **irreducible** chance.
- Godel: truths no proof reaches.
- Turing: undecidability.

---

![bg right:50%](https://upload.wikimedia.org/wikipedia/commons/thumb/4/4c/Ada_Lovelace_daguerreotype_by_Antoine_Claudet_1843_-_cropped.png/330px-Ada_Lovelace_daguerreotype_by_Antoine_Claudet_1843_-_cropped.png)
![bg right:50%](https://upload.wikimedia.org/wikipedia/commons/thumb/9/98/Commodore_Grace_M._Hopper%2C_USN_%28covered%29_head_and_shoulders_crop.jpg/330px-Commodore_Grace_M._Hopper%2C_USN_%28covered%29_head_and_shoulders_crop.jpg)

## algorithm before machine

- Lovelace (1843): the **first algorithm**, no machine yet.
- Hopper (1952): the **compiler** — machines read symbols.
- programming = thinking, **then** running.

---

![bg right:50%](https://upload.wikimedia.org/wikipedia/commons/thumb/c/c1/C.E._Shannon._Tekniska_museet_43069_%282x3_crop%29.jpg/330px-C.E._Shannon._Tekniska_museet_43069_%282x3_crop%29.jpg)
![bg right:50%](https://upload.wikimedia.org/wikipedia/commons/thumb/5/5e/JohnvonNeumann-LosAlamos.gif/330px-JohnvonNeumann-LosAlamos.gif)
![bg right:50%](https://upload.wikimedia.org/wikipedia/commons/thumb/3/3f/EdwardLorenz.jpg/330px-EdwardLorenz.jpg)

## the computational turn

- Shannon: the **bit**.
- von Neumann: Monte Carlo.
- Lorenz: chaos on a computer.

---

![bg right:50%](https://upload.wikimedia.org/wikipedia/commons/4/43/Andrej_Nikolajewitsch_Kolmogorov.jpg)
![bg right:50%](https://upload.wikimedia.org/wikipedia/commons/1/1a/Judea_Pearl_at_NIPS_2013_%2811781981594%29_%28cropped%29.jpg)
![bg right:50%](https://upload.wikimedia.org/wikipedia/commons/thumb/7/71/Gerd_Gigerenzer_%2826970147768%29_%28cropped%29.jpg/330px-Gerd_Gigerenzer_%2826970147768%29_%28cropped%29.jpg)

## where we are now

- Kolmogorov: shortest program wins.
- Pearl: **interventions**, not correlations.
- Gigerenzer: **fast & frugal** heuristics.

---

![bg right:35%](https://upload.wikimedia.org/wikipedia/commons/thumb/4/4d/Herbert_Simon%2C_RIT_NandE_Vol13Num11_1981_Mar19_Complete.jpg/330px-Herbert_Simon%2C_RIT_NandE_Vol13Num11_1981_Mar19_Complete.jpg)

## Simon — bounded rationality

- Simon (1956): **satisficing**. good enough, fast.
- Carnegie school. Nobel 1978.
- the **ancestor** of fast & frugal.
- the demon was always going to lose to a clock.

---

![bg right:35%](https://upload.wikimedia.org/wikipedia/commons/thumb/6/68/Margaret_Hamilton_1995.jpg/330px-Margaret_Hamilton_1995.jpg)

## Hamilton — naming the discipline

- Apollo guidance: software that **had to not fail**.
- coined *software engineering* (~1968).
- error-detection, priority scheduling, recovery.
- SE born **inside** a hard-constrained problem.

---

## SBSE — search lands in SE

- Harman & Jones (2001): SE problems = **search** problems.
- precursor: Clark et al, York (2000) — metaheuristics in SE.
- testing, modules, refactoring -> optimization.
- the SE-domain bet on **scale + search**.

---

<!-- _class: lead -->

![bg right:30%](https://upload.wikimedia.org/wikipedia/commons/thumb/4/4b/SD_2025_-_Richard_Sutton_01_%28cropped%29.jpg/330px-SD_2025_-_Richard_Sutton_01_%28cropped%29.jpg)

## the modern claim

> general methods that **leverage computation** always win.
> cleverness loses to scale + search.

— Sutton, *The Bitter Lesson* (2019)

### ... does it?

---

## the hidden precondition

- search and learning both **spend** evaluations.
- the lesson assumes a **free oracle**: cheap, instant, ~infinite.
- chess/Go: rules give exact win/loss, unlimited self-play.
- speech/vision: labeled corpora, a loss in microseconds.

---

## slow properties have no free oracle

- **maintainability**: signal months later, noisy, can't self-play.
- **reliability**: tail events — needs long runtime or proof.
- **human understanding**: reward = a person gets a testable idea.
- each eval is *slow, costly, human.*

---

<!-- _class: lead -->

## the real axis

not scale vs cleverness.

# <!-- fit --> eval cost × latency × availability

- cheap/instant evals -> Sutton wins.
- expensive/slow/human -> frugal is the **only** thing that runs.

---

## biting the future

- the lesson is a **margin loan** on Moore's law.
- assumes resources keep growing exponentially.
- Dennard dead (~2006). cost/transistor flat past 28nm.
- peak data. megawatt training. returns are power-law.
- exponential -> **logistic**. the collateral is capping.

---

![bg right:35%](https://upload.wikimedia.org/wikipedia/commons/thumb/4/49/Niklaus_Wirth%2C_UrGU.jpg/330px-Niklaus_Wirth%2C_UrGU.jpg)

## Wirth's witness

- reflex: *"write it loose, faster CPUs bail you out."*
- **Wirth's law**: bloat outruns hardware. you fall behind.
- the bet lost **during** the boom — exponential fully running.
- Wirth is the **prosecution**, not the defense.

---

<!-- _class: lead -->

![bg right:30%](https://upload.wikimedia.org/wikipedia/commons/thumb/7/78/William_Stanley_Jevons_portrait_extract.jpg/330px-William_Stanley_Jevons_portrait_extract.jpg)

## Jevons closes the door

> cheaper coal -> **more** coal burned.

— Jevons (1865)

- cheaper compute -> bigger models -> nothing banked.
- frugality is **chosen**, never delivered.

---

## debloat

- every dependency = a margin loan on future **attention**.
- cheap to add (compiles, tests green) — slow to pay (CVEs, churn).
- left-pad / Log4Shell / xz: dep count is now measured risk.
- not *"fewer deps"* — fewer **un-evaluated, low-trust** deps.
- **eval-weighted minimalism.**

---

<!-- _class: lead invert -->

## the thesis

- scale beats cleverness **only** where evaluation is free.
- most of software has **no free oracle**.
- bounded resource, bounded mind, frugal toolbox.

# <!-- fit --> *simple ain't stupid.*

---

<!-- _class: lead invert -->

## close

- Gigerenzer's second revolution, replayed in SE.
- the omniscient demon was always a **dream**.
- build for the world we're in: **bounded, capping, costly to check.**

---

<!-- _class: lead -->

# case study

## can AI be easy?

---

<!-- _class: lead -->

> Before you buy a Ferrari
> to drive to the grocery store,
> **try walking.**

---

![bg right:40%](https://scholar.googleusercontent.com/citations?view_op=view_photo&user=K4teyvoAAAAJ&citpid=10)
![bg right:40%](https://github.com/KKGanguly.png)
![bg right:40%](https://amiiralii.github.io/pic.png)

## why frugal works

- **PROMISETUNE** (Tao Chen): ~5 of N knobs move the metric.
- **BINGO** (Kishan Ganguly, FSE): tiny y-region holds the wins.
- **simpler explanation** (Amirali Rayegan, JSS): minimal data, full story.
- x-space important part: **small**. y-space: **smaller**.

> *the world is simpler than you think.*

---

![bg right:35%](https://srina1h.github.io/images/profile.jpg)

## EZR.py

- **400 lines** of Python. stdlib only. <1MB install.
- 120+ tabular SE tasks: matches/beats SMAC3, SHAP, LIME, FASTREAD.
- **500x faster.** under **100 labels.**

w/ **Srinath Srinivasan**.

---

![bg right:35%](https://upload.wikimedia.org/wikipedia/commons/thumb/a/af/Karen_Sp%C3%A4rck.jpg/330px-Karen_Sp%C3%A4rck.jpg)

## how — strip the redundancy

- read the code. many *"different"* algorithms collapse.
- 4 classes: `Num`, `Sym`, `Cols`, `Data`.
- 1-line flip: decision tree numeric <-> symbolic.
- 1983 Simulated Annealing still beats modern Local Search.
- Spärck Jones (1972) IDF -> 30-line NB beats SVM on text.

---

## six myths

| # | myth | reality |
|---|---|---|
| 1 | need heavy infra | **stdlib** |
| 2 | each task its own algo | **same 4 classes** |
| 3 | trees differ by type | **1-line flip** |
| 4 | newer beats older | **SA'83 wins** |
| 5 | need massive data | **100 labels = 85-95%** |
| 6 | text needs advanced models | **30-line NB beats SVM** |

---

## by the numbers

| metric | value |
|---|---|
| vs. SMAC3 | **500x faster** |
| labels to optimum | **< 100** |
| features used | **< 10** |
| code size | **400 lines** |
| install size | **< 1 MB** |
| tasks tested | **120+** |

---

<!-- _class: lead invert -->

# <!-- fit --> how many complex SE problems...

# <!-- fit --> aren't?

---

<!-- _class: lead -->

## the punchline

- scope: tabular SE. generative (LLMs, images) TBD.
- 30-line NB beating SVM on text hints it extends.
- developers still need to **read code**.

### arXiv:2606.03640
