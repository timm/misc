<!-- chunk: 27_lec2_exercises -->
<!-- prev: 26_main  next: 90_appendix_lua -->

### Lecture 2 exercises

1. Run `lua nb.lua --nb data/diabetes.csv` and pipe the output to
   `awk -F'\t' '$1==$2 {h++} END {print h}'` to count correct
   guesses. Then do the same with `data/soybean.csv`. Why does the
   soybean dataset score higher?
2. The classifier classifies-then-trains, in that order. Change
   the order in [`nb`](#nb) to train-then-classify. Does accuracy
   on diabetes go up or down? Explain in three sentences.
3. Set `the.k = 100` and `the.m = 100` before running.
   ([`Main`](#main) reads them from the help string — edit the
   defaults there.) Accuracy will collapse on diabetes. Why? Use
   the [m-estimate breakout](#breakout-ai-the-m-estimate).

**Homework (due next lecture):** Port [`SYM.like`](#symlike),
[`NUM.like`](#numlike), and [`DATA.likes`](#datalikes) to the same
file you wrote for Lecture 1's homework. Use the prompts you ran
in this lecture as test cases: a column of `{10,20,30,40,50}` must
report a likelihood of about `0.025` at `v=30`. Submit on the LMS.
