# fri.lisp

An active-learning library in 500 lines of Common Lisp. This book walks through it, piece by piece.

[1 motivation](01-motivation.md) · [2 shape](02-shape.md) · [3 stats](03-stats.md) · [4 distance](04-distance.md) · [5 active](05-active.md) · [6 trees](06-trees.md) · [7 lisp](07-lisp.md) · [8 rejected](08-rejected.md) · [9 results](09-results.md)

## Chapters

1. [Motivation](01-motivation.md). Why so little code.
2. [Shape of data](02-shape.md). Num, Sym, Cols, Data. Four verbs: add, sub, mid, spread.
3. [Online stats](03-stats.md). Welford in eight lines. The `Num` as single source of truth.
4. [Distance](04-distance.md). `disty` for goals, `distx` for features. Polymorphism earns its keep.
5. [Active learning](05-active.md). Twenty lines for the whole loop.
6. [Trees and the explanation tax](06-trees.md). Interpretability has a minimum sample size.
7. [Lisp debt repaid](07-lisp.md). Reader macros, anaphora, and why the notation matters.
8. [What we rejected](08-rejected.md). TDD, documenting-everything, and other things we declined.
9. [Results](09-results.md). Numbers, citations, closing.

## Source

- [fri.lisp](../fri.lisp) &mdash; the whole program.
