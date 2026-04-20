[index](index.md) · [1 motivation](01-motivation.md) · [2 shape](02-shape.md) · [3 stats](03-stats.md) · [4 distance](04-distance.md) · [5 active](05-active.md) · [6 trees](06-trees.md) · [7 lisp](07-lisp.md) · [8 rejected](08-rejected.md) · [9 results](09-results.md)

# 1. Motivation

> Email is a wonderful thing for people whose role in life is to be on top of things. But not for me; my role is to be on the bottom of things.
> &mdash; Donald Knuth[^knuth]

In an age where everyone hands the details to an LLM, what are we missing?

We refactored an AI research codebase to the bone. Five hundred lines of Common Lisp, one file, no dependencies, runs in milliseconds. Five things stopped being true.

## One learner, three problems

Textbooks split AI into regression, classification, and optimization. Our single active-learning loop does all three. The difference is a suffix in the CSV header. A column named `Mpg-` is a minimization target. `Mpg+` is maximization. `class!` is a classifier label. Everything else is a feature. Three textbook chapters, one algorithm.

## Five abstractions, everywhere

`Num`, `Sym`, `Cols`, `Data`, and a distance function. Every module in the codebase composes from these. The `Num` that tracks mean and standard deviation is one line of slots:

```lisp
(defstruct (num (:constructor %make-num))
  (at 0) (txt " ") (n 0) (mu 0) (m2 0) (heaven 1))
```

That same `Num` is the beating heart of the active learner, the splitting criterion for decision trees, the basis of every "is X better than Y" judgement the code makes. Build once, reuse always.

## Interpretability is cheap

Rudin argued that high-stakes decisions should use interpretable models, not explained black boxes[^rudin]. We agree. The cost is small.

Our decision tree learner is forty lines of Lisp. Each tree prints on a page:

```
0.35 ( 27)   Mpg <= 25
0.30 ( 13)   |.. Hp <= 100
0.41 ( 14)   Mpg >  25
```

We measured the tax. At thirty labels, a tree-ranked holdout loses six points of win to a centroid baseline. At fifty labels, the tax is zero. Interpretability has a minimum sample size. Above that threshold, it costs nothing.

## Fewer than fifty labels beat big data

Labels in software engineering are expensive. Expert annotation fatigues quickly. Historical logs are unreliable; up to ninety percent of technical-debt entries are false positives[^yu]. Automated oracles use approximate heuristics that miss the interesting cases. Active learning picks the informative rows and stops. Fifty labels outperform baselines trained on thousands.

## Dependencies are a supply-chain bet

Modern software is eighty-five to ninety-seven percent external code[^martinez]. Each dependency widens the attack surface. A single poisoned npm or PyPI update compromises thousands of downstream packages silently.

Richard Hipp, author of SQLite, practices *backpacking*: cutting code to what is essential. He built his own editor, servers, storage engine. He shunned Berkeley DB. Oracle later acquired it and demanded license fees. SQLite ships today on every Android phone, every iPhone, Chrome, Firefox, Safari, most Linux distributions, and Mars rovers.

> If you want to be free, that means doing things yourself.
> &mdash; Richard Hipp[^hipp]

Our CSV reader is five lines. Our CLI parser is twelve. The whole file is five hundred.

## Why Lisp?

Lisp is the source. Macros, closures, garbage collection, REPLs, first-class functions, dynamic typing &mdash; these were Lisp before they were Python, JavaScript, Ruby, or Rust. Reading it changes how you read everything else.

You may not know Lisp. That is fine. The code is short; the shape carries. `defmethod` dispatches on type. A reader macro rewrites its body once, at read time. `let` binds names. Learn the rest as you go.

We treat Lisp as notation. Every chapter ends with a **Try it yourself** list of small exercises you can port to your own language. If you write Python, Go, Rust, or OCaml: implement the exercise there. The ideas are not the syntax.

## The claim

If you cannot see the whole algorithm on one screen, you do not understand it. You trust it. Trust is a bug.

## Try it yourself

**`--the`.** Print the config. In fri.lisp the config lives in a list called `*the*`: each row is `(name default flag description)`. The CLI parser walks command-line arguments, matches flags to rows, and overwrites defaults. No argparse, no dictionary of globals. One list that is both the help text and the storage.

Implement this in your language. A five-element list of four-tuples, a flag parser that finds a row by its third field, and a CLI loop that overwrites the second field. The description field doubles as `--help` output when you want it. The point is to feel how config-as-data cuts out a whole library.

---

[^knuth]: Donald Knuth, *Email (let's drop the hyphen)*, <https://www-cs-faculty.stanford.edu/~knuth/email.html>.
[^rudin]: C. Rudin. "Stop explaining black box machine learning models for high stakes decisions and use interpretable models instead." *Nature Machine Intelligence* 1, 206&ndash;215 (2019). <https://www.nature.com/articles/s42256-019-0048-x>.
[^yu]: Z. Yu et al. "Identifying self-admitted technical debt in open source projects using text mining" (2020).
[^martinez]: Martinez et al. "Studying the external dependencies of open-source systems" (2021).
[^hipp]: Richard Hipp, interview (2021).
