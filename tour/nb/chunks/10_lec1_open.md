<!-- chunk: 10_lec1_open -->
<!-- prev: 03_install  next: 11_sym -->

## Lecture 1: Data engineering

> **REPL prompts covered:** [ref:l1-start]–[ref:l1-end].
>
> **Lecture defers:** nothing. By the end, you will have seen every
> Lua-specific construct used in this lecture explained either in
> place or in a breakout.
>
> **Concepts:** column-oriented summaries, running mean and
> variance (Welford), the column-type dispatch trick, the difference
> between a table of rows and a table of column summaries.

In Lecture 1 we build the data engineering layer. No probabilities,
no Bayes. Just the question: when 700 rows of mixed numbers and
strings come at you one at a time, what is the smallest structure
that lets you keep up?

By the end you will be able to load any CSV with a header row, ask
the data what its columns are, and ask any single column for its
running summary. Lecture 2 then uses those summaries as a
probability model.
