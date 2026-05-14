<!-- chunk: 20_lec2_open -->
<!-- prev: 19_lec1_exercises  next: 21_sym_like -->

## Lecture 2: Naive Bayes

> **REPL prompts covered:** [ref:l2-start]–[ref:l2-end].
>
> **Lecture defers:** nothing inside this file; we will explain
> probability, log-probability, the independence assumption, and
> the m-estimate where they are used.
>
> **Concepts:** likelihood of a value under a column's summary;
> joint likelihood of a row; the naive-Bayes simplifying
> assumption; the most-likely-class rule; interleaved training
> and testing.

Lecture 1 built column summaries. Lecture 2 reads those summaries
as probability models, multiplies them together, and picks the
class whose multiplication wins. That is naive Bayes in one
sentence.

You will see that "classify" and "train" do not look very different.
Both walk rows. Training updates a summary; classifying queries one.
The classifier reuses the same data engineering layer; that is why
we spent Lecture 1 on it.
