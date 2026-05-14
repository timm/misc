# Author Prompt v2: Genetic Stanza, Tutorial Edition

You are authoring a tutorial-style source-and-paper bundle in
"genetic stanza" style adapted for grad SE classroom use. The
deliverable is a `chunks/` directory of single-concept markdown
files plus a tiny build pipeline that assembles them into one
read-through document. K&R chapter 1 is the reference for tone
and pacing; Lions' Commentary is the reference for code-as-paper
coverage; fri2.md is the reference for the stanza+REPL form.

The chunked layout exists so that any single piece can be revised
in isolation with an LLM (one chunk fits comfortably in context),
while the assembled artifact reads top-to-bottom as one document.

---

## 1. CONFIG

Open the project with an AUTHOR-CONFIG block in
`chunks/00_config.md`. Every later decision refers back to it.
The audit question is "did the file match its config?", not "did
this feel right?".

### Required

- **audience** — one sentence pinning reader background.
  e.g. "first-year grad SE student, fluent in one
  imperative language, no Lua."
- **assumed** — comma list of constructs presumed fluent and NOT
  explained anywhere. e.g. "for-loops, if/else, function
  calls, recursion, basic stats (mean, sd)."
- **deferred** — comma list of constructs the body uses
  WITHOUT explaining in place, covered in the appendix or the
  tutorial. Each item gets a half-line note in chunk 01 ("we
  use Lua iterators throughout; appendix L covers them").
  This is the reader's bill of rights, K&R-style.
- **language** — target language + dialect/runtime.
  e.g. "Lua 5.4" or "Common Lisp (CLISP)".
- **source-file** — single file being toured.
  e.g. "src/nb.lua".
- **lectures** — integer, total lecture-sized chunks.
  One lecture ≈ 80 min ≈ 15 REPL prompts ≈ 3–6 stanzas.
- **target** — html | pdf | dual.

### Optional (with defaults)

- depth: terse | standard | verbose [standard]
- tone: kr | textbook | academic | terse [kr]
- voice: you | third | imperative [you]
- prose-width: integer [65]
- code-indent: integer spaces [4]
- numbering: on | off [on]
- repl-start: integer [1]
- repl-density: stanzas between REPL traces [1]
- breakout-budget: max breakouts per lecture [4]
- example-autorun: yes | no [yes]
- cross-links: on | off [on]
- exercises-per-lecture: integer [3]
- tutorial-placement: back | jit | none [back]

### Example config

```
<!-- AUTHOR-CONFIG -->
audience: first-year grad SE student, basic Python or Java,
          no Lua.
assumed:  for-loops, if/else, function calls, recursion,
          mean and standard deviation, naive Bayes
          assumption (independence of features).
deferred: Lua tables and iterators, metatable-based OO,
          CSV reading, the `:` method-call syntax,
          Welford's algorithm, m-estimate smoothing.
language: Lua 5.4
source-file: src/nb.lua
lectures: 6
target:   dual
tone:     kr
voice:    you
```

---

## 2. PROJECT LAYOUT

```
project/
  Makefile
  master.md            # spine: ordered list of chunk includes
  build/
    build.md           # assembled, numbered, TOC injected
    out.html
    out.pdf
  chunks/
    00_config.md       # AUTHOR-CONFIG block (not rendered)
    01_motivation.md   # front matter
    02_peek.md
    03_install.md
    10_lec1_open.md    # lecture openers
    11_sym.md          # stanzas
    12_num.md
    13_col.md
    14_data.md
    15_lec1_exercises.md
    20_lec2_open.md
    ...
    90_appendix_lang.md
    99_examples_index.md
  scripts/
    assemble.awk       # inline includes
    number-repl.awk    # assign global [N]> numbers
    toc.awk            # generate menu bar from ### headings
    xref-check.awk     # verify all anchor links resolve
    used-in.awk        # generate reverse "used in" index
  src/
    nb.lua             # canonical source
```

The chunk file is the canonical edit surface. The build
artifact is downstream. Chunks are numbered with gaps (10, 11,
12, 20, ...) so a new chunk can slot in without renaming the
neighbours.

---

## 3. BUILD PIPELINE

Three required passes plus two checks. Each script ≤ 30 lines of
awk. No framework dependency beyond awk + pandoc.

1. `assemble.awk`: scan `master.md`, replace each line of the
   form `<!-- include chunks/NN_slug.md -->` with the content of
   that chunk file. Output: `build/step1.md`.

2. `number-repl.awk`: scan `step1.md`, find every `[?]>` and
   replace with `[N]>` where N starts at `repl-start` and
   increments. Tutorial-appendix prompts use `[?T]>` and get
   `[T1]>`, `[T2]>`, ... in their own counter. Also resolves
   `<!-- ref:label -->` comments inserted by exercises so "do
   prompts 5–12" stays valid after chunk reordering.

3. `toc.awk`: walk the numbered file, harvest all `###`
   headings, build the menu-bar TOC described in §10. Inject
   it after the `# Title` line.

4. `xref-check.awk`: every `[name](#anchor)` link must resolve
   to an existing `###` heading auto-slug. Fail the build
   otherwise.

5. `used-in.awk`: scan `build.md` for `### name` and for every
   later mention of `name` in prose or code; emit a reverse
   index ("`make-num` used in: 4.2, 5.1, 7.3") as the final
   appendix.

Makefile sketch:

```
SCRIPTS = scripts
build/build.md: master.md chunks/*.md
	awk -f $(SCRIPTS)/assemble.awk    master.md     > build/s1.md
	awk -f $(SCRIPTS)/number-repl.awk build/s1.md   > build/s2.md
	awk -f $(SCRIPTS)/toc.awk         build/s2.md   > build/s3.md
	awk -f $(SCRIPTS)/used-in.awk     build/s3.md   > build/build.md
	awk -f $(SCRIPTS)/xref-check.awk  build/build.md  # exits nonzero on broken

build/out.html: build/build.md
	pandoc build/build.md -o build/out.html --standalone --toc
```

---

## 4. CHUNK FILE CONTRACT

Every chunk file starts with a four-line HTML-comment header
that is NOT rendered but provides context when revising the
chunk in isolation:

```
<!-- chunk: 11_sym -->
<!-- prev: 10_lec1_open  next: 12_num -->
<!-- audience: see chunks/00_config.md -->
<!-- repl: numbers assigned at build -->
```

The LLM-revision workflow: paste config + prev + this chunk +
next chunk. That is the full context any single revision needs.

---

## 5. FRONT MATTER

Three chunks, in order, before the first lecture opener:

### 5.1 Motivation (chunk 01)

Two to four short paragraphs. Why this code is worth reading.
K&R voice — address "you" sometimes, admit limits, give the
reader permission to skim. Should answer: what hurt before,
what did the author do about it, what's the takeaway in one
sentence. If you have an existing intro paragraph from a paper
or grant, mine it; do not invent fresh motivation prose.

### 5.2 Peek (chunk 02)

A sample input and sample output side by side, with one
paragraph of orientation. The reader should be able to point
to "what we are building". No code yet. K&R does this with
`hello, world` and the temperature table. For tabular AI work,
show one MOOT-style CSV and one line of predicted output.

### 5.3 Install (chunk 03)

Three to five lines, copy-pasteable, tested. Fails fast on a
missing dependency. Example:

```
git clone https://github.com/USER/REPO.git
cd REPO && lua src/nb.lua --num
```

If install needs more than five lines, factor the surplus into
an appendix and link to it. The peek + install chunks should
take a new student under five minutes.

---

## 6. LECTURE STRUCTURE

A lecture is a coarser unit than a stanza. Each lecture is a
cluster of 3–6 stanzas plus 0–2 breakouts plus exercises, sized
so a student can complete it in one 80-minute session.

### 6.1 Lecture opener (one chunk)

A numbered heading and four short blocks:

```
## Lecture 1: Atoms

> **REPL prompts covered:** 1–14.
>
> **This lecture defers:** Lua's `:` method-call syntax, the
> metatable-based OO idiom, and the `iter()` / `csv()` helpers
> from `lib.lua`. Each appears in the tutorial appendix (§9).
>
> **Concepts:** running mean (Welford), running mode and
> frequency table, column-type dispatch.

A one-paragraph orientation. What you'll be able to do at the
end of this lecture, in plain English. Three sentences max.
```

The per-lecture deferred-complexity block is the local
extension of the global `deferred:` config. It gives the
reader explicit permission to defer questions until the
appendix.

### 6.2 Lecture body (3–6 stanza chunks + 0–2 breakout chunks)

Stanzas in build order (atoms before containers before
operations before callsites). Breakouts interleaved
immediately after the stanza whose code motivates them.

### 6.3 Lecture closer (one chunk)

Exercises and homework:

```
### Lecture 1 exercises

1. Run prompts 3, 7, and 12 in your local REPL. Paste the
   outputs into a file.
2. Modify the SYM `add` so that values "?" still increment
   `n` but are stored under the key `"?"`. Show the effect on
   prompt 8.
3. Predict what `Sym.like("?", 0.2)` will print given the
   current code, then verify.

**Homework (due next lecture):** Port REPL prompts 1–14 to
Python. Single `.py` file, no third-party libraries. Submit on
the LMS.
```

Three exercises is the default (`exercises-per-lecture: 3`).
Homework is one task that scales the lecture's REPL range to a
language the student is already fluent in — that's the spaced
repetition device.

---

## 7. STANZA FORM

One concept per stanza. Each stanza chunk has:

1. A numbered `###` heading using the identifier name.
   Numbering is on by default and assigned by section/stanza
   position in `master.md`.
2. One prose block, ≤ prose-width (default 65). Names the
   thing, says why it exists, calls out the non-obvious. Skip
   the obvious. Spend prose on design choices and surprises.
3. One fenced code block, language-tagged, lifted verbatim from
   `src/`. No retyping; the source is canonical.
4. (Optional) one REPL trace, 1–4 lines, with `[?]>`
   placeholders the numbering pass will resolve.
5. (Optional) one consequence sentence after the trace,
   pointing forward ("we will use this in `dist`").

Inline cross-links via markdown anchors: every reference to
another defined name in prose is wrapped as
`[name](#name)` so click-to-definition works.

### Example stanza

```
### 4.1 Sym

A `sym` is a column of symbolic values. It keeps a running
count of how often each value appears in its `has` table, and
nothing else. The `n` slot is the total count; `at` and `txt`
remember which column this is for later joining.

​```lua
local function Sym(n,s)
  return isa(SYM,{at=n or 0, txt=s or "", n=0, has={}}) end
​```

​```
[?]> Sym(1, "Color")
{at=1, txt="Color", n=0, has={}}
[?]> local s = Sym(); s:add("red"); s:add("red"); s:add("blue"); s
{at=0, txt="", n=3, has={red=2, blue=1}}
[?]> adds({"a","a","a","b","c"}, Sym())  -- == eg["--sym"]
{at=0, txt="", n=5, has={a=3, b=1, c=1}}
​```

The pattern (constructor, then `add` builds the summary one
value at a time) repeats for [Num](#num) below.
```

---

## 8. BUILD ORDER

Concepts appear in the order they could be discovered.

1. Atoms first (leaf data structures).
2. Sibling atoms paired before either is used (Num + Sym
   together, not Num then ten stanzas then Sym).
3. Operations over atoms after the atoms exist.
4. Containers after the things they contain.
5. Factories after their products.
6. Call sites last, as payoff.

Never reference a code name before it is defined in the body.
Forward concept references are OK ("we'll use this in the tree
later"); forward code references are not.

---

## 9. SIBLING PAIRING

Mirrored concepts go adjacent with parallel prose. Num/Sym.
mid/spread. mu/mode. add/sub. like-Sym/like-Num. Each pair
of stanzas reads with the same shape so the reader notices the
symmetry. Symmetry is a teaching device; expose it.

---

## 10. BREAKOUT BOXES

Inline asides, typographically distinct (blockquote bold-titled
in markdown). Three flavours, never mixed:

- **SE concept** — DRY, SOLID, single responsibility, premature
  optimization, separation of concerns. Use when the
  surrounding code carries a software-engineering point worth
  one paragraph.
- **AI concept** — entropy, Welford, m-estimate, Laplace
  smoothing, the naive Bayes assumption, etc. Use when the
  code embeds an algorithmic idea that needs one paragraph of
  background to land.
- **Language concept** — Python yield, Lua metatables, Lisp
  setf, etc. Use when the code uses a construct the assumed
  background does not cover, AND covering it inline is shorter
  than a tutorial-appendix detour.

### Form

```
> **Breakout (AI): Welford's algorithm.**
> Computing variance in one pass without storing all values.
> Keep `mu` (running mean) and `m2` (sum of squared deviations
> from the running mean). On each new value `v`: let
> `d = v - mu`, set `mu += d / n`, then `m2 += d * (v - mu)`.
> `sd` recovers as `sqrt(m2 / (n-1))`. That is exactly the
> three lines of `NUM.add`, with no list of seen values
> retained.
```

### Rules

- ≤ 8 lines of prose per breakout
- placed immediately after the stanza whose code motivates it
- self-contained: reader can skip it and the body still flows
- budget: `breakout-budget` per lecture (default 4)
- each breakout gets a stable anchor
  (`### Breakout: Welford {#bo-welford}`) so other stanzas can
  link to it
- breakouts live in their own chunk files
  (`12a_bo_welford.md`) so they are revisable independently

---

## 11. REPL TRACES

The body's pedagogical view of the executable truth in chunk 99
(the eg index).

### Numbering

Authors write `[?]>` in chunk source. `number-repl.awk` assigns
global numbers at build time. Sequential across the whole body
(`[1]>`, `[2]>`, ..., `[N]>`). The tutorial appendix uses
`[?T]>` with its own counter (`[T1]>`, `[T2]>`, ...).

This means: when you insert a new REPL line in stanza 4, every
later prompt renumbers automatically. Exercises that say "run
prompt 12" use a label, not a literal number:

```
<!-- ref:demo-sym -->
[?]> adds({"a","a","a","b","c"}, Sym())  -- == eg["--sym"]
```

```
Exercise: re-run [ref:demo-sym] with a missing value.
```

`number-repl.awk` rewrites `[ref:demo-sym]` to whatever number
that prompt got at build time.

### Tying REPL to eg

Every eg-function-equivalent line in a REPL trace gets a
trailing comment `-- == eg["--name"]`. The build script can
then verify (a) every eg has at least one tagged REPL line and
(b) the captured stdout from running the eg matches the shown
output. Stale traces fail the build.

Atomic precursor lines (the ones not tagged) are pedagogical
only: they demonstrate the just-defined function in isolation.
The eg-tagged line is the canonical demo and the regression
test.

---

## 12. EXAMPLES INDEX (chunk 99)

Lift from the source if it maintains an eg table (nb.lua has
`eg = {}`; ezr.py has `eg.*` functions). Otherwise hand-write
one `eg["--name"]` per concept introduced in the body. Each:

- is a one-line printable demonstration
- owns its own state (no shared globals)
- maps to a CLI flag (`lua nb.lua --sym`)
- appears verbatim, with `-- == eg["--name"]` tag, on exactly
  one body REPL line

Plus a `eg["--all"]` that runs every named example.

The eg index is the executable truth. The body's REPL traces
are the reading view. Tooling generates one from the other.

---

## 13. TUTORIAL APPENDIX

A short tour of the language constructs the body uses but does
NOT explain inline. Everything in `assumed:` is skipped here.
Everything in `deferred:` is covered here. Anything else the
body uses must appear here too.

Same stanza rules (width, indent, one concept per `###`).
Examples lifted from the body, not invented toys. Each entry
cites the body stanza where the construct first appears, and
the body links forward to the tutorial entry.

### Two-pass audit (run before delivery)

1. **Coverage.** Walk every distinct construct in the body.
   Each must appear in the tutorial OR in `assumed:`. No
   exceptions.
2. **Necessity.** Walk every tutorial entry. Each must appear
   in the body. Cut dead entries.

Audit output: a diff list (add / cut), not a verdict.

### Per-language confusion checklist (Lua shown)

- `:` method-call syntax vs `.` field access
- metatables and `setmetatable` / `__index`
- the OO idiom via metatables (no `class` keyword)
- iterators and the for-in protocol
- numeric vs generic `for`
- `nil` as both missing-value and false
- string library: `find`, `match`, `gmatch`, Lua patterns
  (not regex)
- tables as both arrays and hashes
- `pairs` vs `ipairs`
- multiple return values
- closures and upvalues
- `require` and module conventions

Adapt the checklist for the target language. Audit fails if
the body uses any item not covered AND not in `assumed:`.

---

## 14. CROSS-REFERENCING

### Forward (click-through)

Every reference in prose to a defined name is wrapped as
`[name](#name)` where the anchor is the markdown auto-slug of
the `###` heading. `xref-check.awk` verifies all links
resolve.

This replaces the Lions-style "see line 2864" device. With
hypertext, the anchor IS the line reference.

### Reverse (auto-generated)

`used-in.awk` scans `build.md` for every `### name` heading
and for every subsequent mention of `name` in prose or code,
emitting:

```
make-num    used in: §4.2, §5.1, §7.3
add         used in: §4.1, §4.2, §5.1, §5.2, §7.1, ...
```

Rendered as the final appendix in HTML and PDF alike. This
preserves the function of Lions' back-of-book index without
the manual labour. Do not hand-maintain this section.

---

## 15. VOICE / TONE

Default `tone: kr`. K&R chapter 1 is the reference. Pin:

- address the reader as "you" sometimes, not constantly
- admit limits up front ("the complete story is in §N")
- short concrete sentences
- examples beat definitions; definition follows the second use
- avoid jargon-shed words ("leverage", "facilitate",
  "robust solution")
- mild dryness OK; no jokes

### Sample of K&R voice

> The only way to learn a new language is to write programs in
> it. The first one is the same for all languages — print
> `hello, world`. This is a bigger hurdle than it sounds; you
> have to create the program text, compile it, run it, and
> find out where the output went. With these mechanical details
> mastered, everything else is easy.

### Other tones

- `textbook`: longer sentences, "you" drops to occasional,
  more signposting ("recall from §3 that...")
- `academic`: third-person, hedged, citation-rich. main-v6.tex
  is the reference.
- `terse`: fragments OK, minimal explanation. fri2.md is the
  reference.

---

## 16. OUTPUT INVARIANTS

Before delivery, the build must pass each check below. `make
check` runs them all.

1. `make` produces `build/build.md`, `build/out.html`,
   `build/out.pdf` with no errors.
2. AUTHOR-CONFIG block exists in `chunks/00_config.md` and
   declares all required fields.
3. Every chunk in `chunks/` has a four-line context header.
4. Every chunk is referenced from `master.md`.
5. Every include in `master.md` points to an existing chunk.
6. Build order matches the genetic rule (atoms first, etc.).
7. No identifier appears in code before it is defined in the
   body.
8. Every name in prose appears in nearby code (≤ 1 stanza
   away).
9. Width: prose ≤ `prose-width`, code unchanged from source.
10. REPL prompts numbered 1..N globally with no gaps;
    appendix uses its own T1..TM counter.
11. Every eg function appears as exactly one REPL line tagged
    `-- == eg["--name"]`.
12. `make repl-check` runs each eg, captures stdout, diffs
    against the corresponding body REPL output. Zero diffs
    required.
13. Tutorial appendix passes coverage + necessity audit.
14. Every breakout has a stable anchor and is within the
    per-lecture budget.
15. Every lecture opener names its REPL range and its
    per-lecture deferrals.
16. Every lecture closer has `exercises-per-lecture` exercises
    plus one homework.
17. `xref-check.awk` reports zero broken anchors.
18. The reverse "used in" index is regenerated, not
    hand-edited.

---

## 17. WHAT NOT TO DO

- Do not write top-down stepwise refinement. That is
  Knuth/Wirth; this is the inverse.
- Do not split a struct/class definition across stanzas.
- Do not paste a whole file and narrate it. One stanza at a
  time, in build order.
- Do not invent REPL output. If it wasn't captured from a real
  run, it is wrong by default.
- Do not invent eg functions. Lift them from the source. If
  the source lacks them, add them to the source first, then
  cite them in the body.
- Do not use a breakout to dump a textbook chapter. 8 lines
  max. If it grows, promote it to its own appendix entry.
- Do not skip the deferred-complexity contract. It is the
  reader's bill of rights.
- Do not hand-maintain the menu, the reverse index, or the
  REPL numbers. All three are generated.
- Do not put bullet-pointed exhaustive lists in body prose.
  Body is prose with names; lists belong in lecture openers
  and appendices.
- Do not use docstrings as a substitute for stanza prose. The
  prose lives in the markdown above the code.

---

## 18. DELIVERABLE

A `chunks/` directory of single-concept markdown files; a
`master.md` spine listing them in order; a `Makefile`; five
small awk scripts in `scripts/`; and a single `src/<file>` that
the body tours and the eg index exercises. `make` assembles the
chunks into one read-through HTML (and optionally PDF). The
chunks are the canonical edit surface; the assembled build is
downstream.

The same body of code is both the program and the tutorial.
Editing one chunk is a small LLM conversation. Editing the
whole tutorial is `make`.
