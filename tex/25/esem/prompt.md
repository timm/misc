# Research Paper to Information-Dense Slides Prompt

## OBJECTIVE
Convert an academic research paper into Beamer/LaTeX slides that are **information-rich** with **reserved visual space** for graphics to be added later.

## CORE PHILOSOPHY
- **Dense information is GOOD** - slides should explode with things to read
- **Visual space is MANDATORY** - every slide reserves space for graphics (even if added later)
- **Authority through citations** - liberal use of inline citations in **bold [brackets]**
- **Memorable through laws/principles** - distill findings into quotable statements
- **Prominent download links** - make PDF accessible throughout
- **ASCII ONLY** - absolutely no Unicode characters (no em-dashes, smart quotes, accented letters, arrows)
- **Slide 2 is always the goal** - mandatory research goal statement in prescribed format

## SLIDE ARCHITECTURE RULES

### 1. Slide Layout Pattern (No Columns - Better Approach)

**DO NOT use two-column layout** - it causes centering issues when graphics are missing.

Instead, use this single-column pattern that works with or without graphics:

```markdown
## [Slide Title]

[DENSE BULLET POINTS HERE - start immediately, no spacing]
- 6-8 bullets are fine
- Include **inline citations [Author21]**
- Bold key terms liberally
- Use nested sub-bullets
- Pack in the information
- More bullets
- Even more bullets

\vspace{1em}
\textcolor{gray}{\footnotesize [Graphic: specific suggestion for what to add - bar chart, diagram, photo, etc.]}
```

**Why this works better:**
- Text starts at top of slide (no centering issues)
- Dense bullets fill the slide naturally
- Graphic suggestion at bottom doesn't create whitespace
- When graphic is added later, it goes at bottom with proper spacing
- `\vspace{1em}` provides spacing before graphic placeholder

**Alternative for slides that MUST have graphics:**

If you're adding graphics immediately (not later), use this:

```markdown
## [Slide Title]

[DENSE BULLETS - 4-6 lines]

\vspace{0.5em}

\includegraphics[height=4cm]{filename.png}
\textcolor{gray}{\tiny Caption or description}
```

### 2. Visual Space Guidelines

**Key principles:**
- **Text fills from top** - no centering, no wasted whitespace
- **Graphic placeholder at bottom** - with 1em spacing above
- **Dense bullets carry the slide** - 6-8 bullets look good even without graphics
- **When graphic added** - it replaces the gray placeholder text at bottom

**Spacing rules:**
- Start bullets immediately after title (no `\vspace` at top)
- End bullets, then `\vspace{1em}` before graphic placeholder
- Graphic placeholder is just gray text suggestion (not a box or space-eater)

**Example of properly filled slide:**
```markdown
## Menzies's 1st Law: Different Projects, Different Metrics

**"Different projects have different best metrics."**

**Evidence:**

- Feature pruning on **3 dozen metrics, 7 NASA datasets**
- Selected just **2-3 attributes per dataset**
- **No single attribute** dominated across projects
- McCabe != Halstead != LOC across different projects
- 1990s theoretical metric debates empirically unfounded **[Fen94]**

**Corollary:** *"Gather all cheaply, then prune irrelevancies."*

**Impact:**

- Changed methodology from "careful selection" to "gather everything, prune later"
- Process metrics might matter more **[Maj24]**, **[Rah14]**

\vspace{1em}
\textcolor{gray}{\footnotesize [Graphic: Heatmap showing metric selection across 7 datasets; or Venn diagram with minimal overlap between dataset preferences]}
```

**What NOT to do:**
```markdown
## Bad Example

\vspace{2em}  <!-- WRONG: Creates whitespace at top -->

- Only 3 bullets
- Not enough content
- Slide looks empty

\vspace{5cm}  <!-- WRONG: Creates huge gap -->
[Graphic placeholder]
```

### 2.5. MANDATORY Slide 2: Research Goal Statement

**CRITICAL: Slide 2 must ALWAYS follow this exact format:**

```markdown
## Research Goal

The goal of this research is to aid **[stakeholder]** to **[solve what problem]** through **[research approach]**.

**Context:**

- [Additional context about the problem]
- [Why this matters]
- [Current limitations]

**Approach:**

- [Key method 1]
- [Key method 2]
- [Key innovation]

\vspace{1em}
\textcolor{gray}{\footnotesize [Graphic: Problem-solution diagram; stakeholder illustration; before/after comparison]}
```

**Examples of goal statements:**
- "aid **software managers** to **reduce post-release defects** through **static code analysis and machine learning**"
- "aid **researchers** to **conduct reproducible studies** through **open data repositories and standardized protocols**"
- "aid **developers** to **optimize hyperparameters efficiently** through **landscape analysis and active learning**"

**Full example:**
```markdown
## Research Goal

The goal of this research is to aid **software engineering researchers** to **build reproducible defect prediction models** through **open data repositories and standardized evaluation protocols**.

**Context:**

- Defect prediction research lacked reproducibility (no shared data)
- Industry skeptical: "no one will give you data" **[Briand06]**
- Each study used different datasets, preventing comparison

**Approach:**

- Created PROMISE repository with 100+ datasets
- Established baseline evaluation methodology
- Made all scripts and data publicly available

\vspace{1em}
\textcolor{gray}{\footnotesize [Graphic: Before/after comparison - isolated researchers vs connected community with shared repository]}
```

### 2.9. ASCII-ONLY CHARACTER RULES (CRITICAL)

**FORBIDDEN CHARACTERS - Never use these:**
- Em-dash (—) → Use: hyphen (-) or "to" or semicolon (;)
- En-dash (–) → Use: hyphen (-) or "to"
- Smart quotes (" " ' ') → Use: straight quotes (" ')
- Ellipsis (…) → Use: three periods (...)
- Arrows (← → ↑ ↓) → Use: words ("<-" "->" "up" "down") or just omit
- Accented letters (é à ñ ü) → Use: plain ASCII (e a n u) or spell out
- Multiplication (×) → Use: "x" or "times"
- Minus sign (−) → Use: hyphen (-)
- Bullet points (•) → Use: markdown hyphen (-)
- Special spaces (non-breaking, thin) → Use: regular space
- Degree symbol (°) → Use: "degrees"
- Copyright/trademark (© ™ ®) → Spell out
- Math symbols (≤ ≥ ≠ ≈) → Use: "<=" ">=" "!=" "~="
- Greek letters (α β γ) → Spell out: "alpha" "beta" "gamma"

**CORRECT REPLACEMENTS:**

Bad: `"Turkish toasters → NASA satellites"`  
Good: `"Turkish toasters to NASA satellites"` or `"Turkish toasters --> NASA satellites"`

Bad: `"2005 → 2007 → 2025"`  
Good: `"2005 to 2007 to 2025"` or `"2005-2007-2025"`

Bad: `"Rodríguez-Pérez"`  
Good: `"Rodriguez-Perez"`

Bad: `"It's a "success" — really!"`  
Good: `"It's a \"success\" - really!"`

Bad: `"10% improvement… significant"`  
Good: `"10% improvement... significant"`

Bad: `"F1 ≥ 0.85"`  
Good: `"F1 >= 0.85"`

**VERIFICATION CHECKLIST:**
- [ ] No em-dashes or en-dashes (search for: — –)
- [ ] No smart quotes (search for: " " ' ')
- [ ] No arrows (search for: → ← ↑ ↓)
- [ ] No accented letters (search for common: é à ñ ü ö)
- [ ] No special math symbols (search for: × − ≤ ≥ ≠)
- [ ] No ellipsis character (search for: …)
- [ ] All quotes are straight: " and '
- [ ] All dashes are ASCII hyphen: -

### 3. Download Link Prominence

**In Title Slide:**
```markdown
subtitle: |
  [Original subtitle]\
  \vspace{0.5em}\
  \textcolor{myred}{\Large\textbf{↓ PDF: [yoururl].pdf}}
```

**In Footer (every slide):**
```latex
- \setbeamertemplate{footline}{
    \hfill
    \textcolor{myred}{\textbf{[yoururl].pdf}}
    \hfill
    \textcolor{linkblue}{\insertframenumber/\inserttotalframenumber}
    \hspace{1em}\vspace{0.5em}
  }
```

**Reminder Slides (every 10-15 slides):**
```markdown
---

## \textcolor{myred}{↓ Download These Slides}

\begin{center}
\Huge\textbf{[yoururl].pdf}

\vspace{2em}

\Large (References clickable in PDF)
\end{center}

---
```

## CONTENT TRANSFORMATION RULES

### 4. Extract "Laws" or "Principles"
From paper findings, create memorable, quotable statements:

**Format:**
```markdown
## Author's [Nth] Law: [Short Title]

**"[Pithy 7-15 word statement that sounds provocative]"**

**Evidence:**

- **Citation [Auth20]**: key finding
- Expected: [conventional wisdom]
- Reality: [surprising result]
- **Implication**: [broader meaning]

**Impact:**

- [consequence 1]
- [consequence 2]
- [practical application]

\vspace{1em}
\textcolor{gray}{\footnotesize [Graphic: diagram showing [specific concept], or meme format comparing expectation vs reality]}
```

**Complete example:**
```markdown
## Menzies's 3rd Law: Turkish Toasters Predict Space Satellites

**"Turkish toasters can predict errors in deep space satellites."**

**Evidence - Transfer Learning Surprises:**

- **Turhan et al. [Tur09]**: Models from **Turkish white goods** predicted **NASA systems** errors
- **Expected**: Complex multi-dimensional transforms needed
- **Reality**: Simple **nearest neighboring** between test/training data worked perfectly
- **Implication**: "Many distinctions made about software are spurious"

**Broader Transfer Learning Success:**

- Cross-domain prediction works better than expected **[Kri19]**, **[Nam18]**
- Suggests **universal patterns** in software defect manifestation
- Questions assumptions about domain-specific modeling requirements

**Why This Works:**

- **Power laws** in software data **[Lin15]**
- **Large repeated structures** in code **[Hin12]**
- Software's inherent "naturalness"

\vspace{1em}
\textcolor{gray}{\footnotesize [Graphic: Split image of toaster + satellite with equals sign; or transfer learning diagram showing domain A to domain B with simple nearest neighbor]}
```

**Examples of good "laws":**
- "Turkish toasters can predict errors in deep space satellites"
- "For SE, the best thing to do with most data is to throw it away"
- "Bigger is not necessarily better"
- "Many hard problems, aren't"
- "Bad learners can make good conclusions"

### 5. Reference Slide Format

**References CAN use two-column layout** (this is one case where columns work well):

```markdown
## Selected References (1/2)

\begin{columns}[T]
\begin{column}{0.48\textwidth}

\small

**[Abb20]:** Abbreviation, A. et al., "Title here," *Venue*, vol. X, pp. Y-Z, Year.  
**[Bak21]:** Baker, B. and C. Charlie, "Another title," in *Proc. CONF*, 2021, pp. 1-10.  
[... 12-15 more references ...]

\end{column}

\begin{column}{0.48\textwidth}

\small

**[Car22]:** Carson, C. et al., "Title," *Journal*, Year.  
**[Dan19]:** Daniels, D., "Title," *Venue*, 2019.  
[... 12-15 more references ...]

\end{column}
\end{columns}

\vspace{0.5em}
\begin{center}
\textcolor{myred}{\textbf{Full bibliography: [yoururl].pdf}}
\end{center}
```

**Why columns work for references:**
- References are short, uniform entries
- Two columns fit more references per slide
- No whitespace issues (all entries same height)
- Makes better use of horizontal space

### 6. Citation Density Pattern
- **Inline format**: `**Author et al. [Auth20]**` (bold name AND citation)
- **Use liberally**: 3-7 citations per slide is normal
- **Stack citations**: `**[Ham09], [Ost04], [Mis11]**` when multiple support one point
- **Reference slides**: Two-column layout with 30-40 references split across columns

### 7. Narrative Arc Structure

**Slide 1 (Title):**
- Title, subtitle with download link
- Author, institution, credentials
- Date

**Slide 2 (MANDATORY Research Goal):**
- MUST use the format: "aid [stakeholder] to [problem] through [approach]"
- Context bullets explaining the problem
- Approach bullets explaining the method
- Graphic suggestion

**Opening (2-4 more slides after goal):**
- Personal/historical context (humanize the work)
- The "radical idea" that was resisted
- Quote skeptics by name

**Core Contribution (5-10 slides):**
- Research question
- Counter-arguments addressed
- Key findings as "laws" or principles
- Supporting evidence (dense citations)

**Impact & Evolution (5-8 slides):**
- Citation metrics / adoption stats
- Extensions and applications
- Surprising findings (the "strange things")

**Current State (3-5 slides):**
- What's hot now
- What needs fixing
- Challenge to community

**References (2 slides):**
- Two-column layout
- 15-20 refs per column
- Download link reminder at bottom

### 8. Title/Subtitle Hierarchy

**Use this pattern consistently:**
```markdown
## Main Slide Title (H2)

### **Key Statement or Law** (H3, bold)

**Section Headers** (bold, no markdown header)

- Bullet points (standard)
  - Sub-bullets (indented)
```

**Don't mix:** `##` with `###` on same line, or have double spaces like `##  `

### 9. Quote Integration
Use italicized quotes strategically:

```markdown
- *"Direct quote from paper that's provocative"* **[Auth20]**
- **Key Finding**: Multi-attribute models outperformed single-attribute models
```

### 10. Humor/Tone Insertion Points
- **Slide titles** can be playful: "Party time in metrics town"
- **Law statements** can be provocative
- **Section names** can use phases: "From 'Good luck with that!' to 'A graveyard of progress'"
- **BUT** keep evidence sections formal and citation-heavy

### 11. Visual Placeholder Suggestions

For each graphic space, suggest **specific** image types:

**Good suggestions:**
```markdown
\textcolor{gray}{\footnotesize
*Suggested visuals:*\
- *Timeline: 2005 → 2007 → 2025 milestones*\
- *Bar chart: citation growth per year*\
- *Photo: original research team at Portland*
}
```

**Bad suggestions:**
```markdown
*Add a relevant image here*  [TOO VAGUE]
```

**Image categories to suggest:**
- **Data visualizations**: graphs, charts, scatter plots
- **Diagrams**: flowcharts, Venn diagrams, system architectures  
- **Photos**: people, places, equipment (humanizing)
- **Screenshots**: tools, interfaces, code
- **Memes/Humor**: expectation vs reality, "is this a butterfly?"
- **Timelines**: historical progression
- **Comparisons**: before/after, vs. competitor

## BEAMER PREAMBLE TEMPLATE

```yaml
---
title: |
   [Main Title]\
   [Optional subtitle line]
subtitle: |
  ([Parenthetical context])\
  \vspace{0.5em}\
  \textcolor{myred}{\Large\textbf{↓ PDF: [yoururl].pdf}}
author: [Author Name]
institute: |
  [title], [dept], \textcolor{myred}{{\bf [institution]}}\
  [credentials/fellowships]\
  [email]\
  [personal url]
date: [Date]
slide-level: 2
fontsize: 9pt
theme: Warsaw
colortheme: default
header-includes:
  - \titlegraphic{\vspace{-5mm}\includegraphics[height=3cm]{logo.png}}
  - \usepackage[sfdefault,light]{FiraSans}
  - \usepackage{microtype}
  - \definecolor{LogicBlue}{RGB}{204,0,0}
  - \definecolor{InferenceRed}{RGB}{212,55,59}
  - \definecolor{linkblue}{HTML}{0066FF}
  - \definecolor{myred}{HTML}{CC0000}
  - \setbeamercolor{structure}{fg=InferenceRed}
  - \setbeamercolor{frametitle}{bg=LogicBlue,fg=white}
  - \setbeamercolor{palette primary}{bg=LogicBlue,fg=white}
  - \setbeamercolor{palette secondary}{bg=InferenceRed,fg=white}
  - \setbeamertemplate{navigation symbols}{}
  - \setbeamertemplate{footline}{\hfill\textcolor{myred}{\textbf{[yoururl].pdf}}\hfill\textcolor{linkblue}{\insertframenumber/\inserttotalframenumber}\hspace{1em}\vspace{0.5em}}
  - \setbeamercolor{block title}{bg=myred!20, fg=myred!80!black}
  - \setbeamercolor{block body}{bg=myred!5, fg=black}
  - \setbeamercolor{background canvas}{bg=gray!2}
  - \newcommand{\graphicspace}[1]{\vspace{1em}\textcolor{gray}{\footnotesize\textit{[Graphic: #1]}}\vspace{3cm}}
---
```

## REFERENCE SLIDE FORMAT

**References use two-column layout** (columns work well here):

```markdown
## Selected References (1/2)

\begin{columns}[T]
\begin{column}{0.48\textwidth}

\tiny

**[Abb20]:** Abbreviation, A. et al., "Title here," *Venue*, vol. X, pp. Y-Z, Year.  
**[Bak21]:** Baker, B. and C. Charlie, "Another title," in *Proc. CONF*, 2021, pp. 1-10.  
[... 15 more references ...]

\end{column}

\begin{column}{0.48\textwidth}

\tiny

**[Car22]:** Carson, C. et al., "Title," *Journal*, Year.  
**[Dan19]:** Daniels, D., "Title," *Venue*, 2019.  
[... 15 more references ...]

\end{column}
\end{columns}

\vspace{0.5em}
\begin{center}
\textcolor{myred}{\textbf{Full bibliography: [yoururl].pdf}}
\end{center}
```

## SPECIAL SLIDE TYPES

### Success Metrics Slide

```markdown
## [Impact Title]

**Citation Impact:**

- **Year X**: Most cited paper in [field]
- **Year Y**: N% of [venue] papers used this
- **Current**: [total] citations

**Industrial Adoption:**

- **Company A [Auth20]**: X% improvement in Y
- **Company B [Auth21]**: Reduced Z by W%

**Commercial Applications:**

- [Application 1 with results]
- [Application 2 with results]

\vspace{1em}
\textcolor{gray}{\footnotesize [Graphic: Citation growth curve over time, or adoption funnel diagram showing progression]}
```

### Controversy/Problem Slide

```markdown
## The Four Phases of [Topic] Evolution

**Phase Evolution:**

1. *"[Skeptical quote]"* - [Description of resistance]
2. *"[Grudging quote]"* - [Description of acceptance]  
3. *"[Acceptance quote]"* - [Description of adoption]
4. *"[Warning quote]"* - [Description of problems]

**The Problem:**

- [Issue 1 with current state]
- [Issue 2 with stagnation]
- **Action Taken**: [Response or solution]

**Moving Forward:**

- [What needs to change]
- [New approaches needed]

\vspace{1em}
\textcolor{gray}{\footnotesize [Graphic: Cycle diagram or timeline showing 4 phases with icons; or s-curve adoption chart]}
```

## CHECKLIST FOR GENERATED SLIDES

- [ ] **Slide 2 is research goal** in format: "aid [stakeholder] to [problem] through [approach]"
- [ ] **ASCII-only characters** throughout (no em-dash, smart quotes, arrows, accents)
- [ ] **No two-column layouts** for content slides (only for reference slides)
- [ ] **Text starts at top** of each slide (no \vspace before first bullet)
- [ ] **6-8 dense bullets** per slide with heavy citation use
- [ ] **Graphic placeholder at bottom** with 1em spacing above
- [ ] Download link prominent in: title, footer, reminder slides
- [ ] 3-5 "Laws" or memorable principles extracted
- [ ] Bold formatting on: **names**, **[citations]**, **key terms**
- [ ] Italics only for: *quotes* and *graphic suggestions*
- [ ] Consistent H2 (`##`) for slide titles, H3 (`###`) for key statements
- [ ] References use two-column layout (2-3 slides)
- [ ] Narrative arc: Goal -> Origin -> Contribution -> Impact -> Current -> Challenge
- [ ] No orphaned `\` or `<br>` tags
- [ ] At least one "Download Reminder" slide mid-deck
- [ ] Personal/historical opening (humanize the research)
- [ ] Provocative challenge/question at end
- [ ] All quotes use straight quotes: " and '
- [ ] All dashes are hyphens: -
- [ ] No Unicode arrows, use "to" or "->" 
- [ ] Names with accents converted to ASCII (Rodriguez not Rodriguez)

## EXAMPLE TRANSFORMATION

**FROM (Paper Abstract):**
> "We investigated the effectiveness of machine learning models for defect prediction using data from 50 projects. Our results show that ensemble methods outperform single classifiers by 15-23% in F1 score."

**TO (Slide):**

```markdown
## Author's Law: Ensembles Beat Soloists

**"In defect prediction, the choir always beats the soloist."**

**Evidence: 50-Project Study:**

- **Data**: 50 industrial projects **[Auth24]**
- **Compared**: Single classifiers (decision trees, logistic regression) vs. ensembles (random forests, gradient boosting)
- **Result**: Ensembles won by **15-23% (F1 score)** across all projects
- **Surprising**: Simple voting schemes outperformed complex stacking methods
- **Consistency**: Advantage held across different domains and project sizes

**Practical Impact:**

- **Recommendation**: Use Random Forests, not single decision trees
- **Production systems**: Gradient boosting for best performance
- **Cost**: Minimal (training time 2x, inference 1.5x) - worth the gain
- **Adoption**: Now standard practice in SE defect prediction **[Gho15]**

\vspace{1em}
\textcolor{gray}{\footnotesize [Graphic: Box plot showing F1 scores - single classifiers (lower, more variance) vs ensemble methods (higher, tight distribution) across 50 projects; or metaphor image of solo singer vs choir]}
```

**Why this works:**
- Text fills slide naturally from top
- 8 dense bullets with citations
- Quotable law statement
- Graphic suggestion specific and helpful
- No wasted whitespace
- Easy to add graphic later (replaces gray text at bottom)

## USAGE INSTRUCTIONS

When given a research paper:

1. **Extract narrative**: Find the human story (origin, resistance, breakthrough)
2. **Identify stakeholder and problem**: For mandatory Slide 2 goal statement
3. **Identify 3-5 core contributions**: Turn these into "Laws" or principles
4. **Harvest citations**: Note all key papers mentioned
5. **Map to slide structure**: 
   - Slide 1: Title with download link
   - **Slide 2: Research goal (MANDATORY format)**
   - 2-4 intro slides (story)
   - 5-10 contribution slides (findings)
   - 5-8 impact slides (adoption, extensions)
   - 3-5 current/future slides (challenges, hot topics)
   - 2 reference slides
6. **For each slide**: Use single-column layout with text starting at top, graphic placeholder at bottom with 1em spacing
7. **Add placeholders**: Specific suggestions for every graphic space
8. **Emphasize download**: Title, footer, mid-deck reminder
9. **Bold liberally**: Names, citations, key terms
10. **Make it quotable**: Extract pithy statements that sound provocative
11. **End with challenge**: Question to audience about assumptions
12. **ASCII-only pass**: Remove all em-dashes (to -), smart quotes (to "), arrows (to "to" or "->"), accents (to plain letters)
13. **Verify Slide 2**: Confirm it uses "aid [stakeholder] to [problem] through [approach]" format

---

## OUTPUT FORMAT

Provide complete Beamer markdown with:
- Full YAML preamble
- **Slide 2: Research goal in mandatory "aid [stakeholder] to [problem] through [approach]" format**
- **Single-column layout** for all content slides (text fills from top, graphic placeholder at bottom)
- **Two-column layout** only for reference slides
- Graphic placeholders at bottom with specific suggestions (1em spacing above)
- Dense citations throughout (6-8 bullets per slide)
- Reference slides at end (two-column format)
- Consistent formatting (no `\` orphans, proper H2/H3)
- **ASCII-only characters** (no em-dashes, smart quotes, arrows, accents)

**FINAL ASCII VERIFICATION BEFORE DELIVERY:**

Search and replace these forbidden characters:
- — (em-dash) -> - (hyphen)
- – (en-dash) -> - (hyphen)  
- " " (smart quotes) -> " " (straight quotes)
- ' ' (smart quotes) -> ' ' (straight apostrophes)
- → ← ↑ ↓ (arrows) -> "to", "from", "up", "down" or "->"
- … (ellipsis) -> ... (three periods)
- é à ñ ü ö (accents) -> e a n u o (plain ASCII)
- × (times) -> x
- − (minus) -> - (hyphen)

**Target length**: 25-40 slides depending on paper complexity.

---

*Save this prompt. Feed it any research paper. Get information-dense slides with visual space architecture.*
