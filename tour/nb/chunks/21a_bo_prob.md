<!-- chunk: 21a_bo_prob -->
<!-- prev: 21_sym_like  next: 21b_bo_mestimate -->

### Breakout (AI): Probability

> A *probability* is a number between 0 and 1 that says "how often,
> in the long run, does this happen". `P(red) = 0.6` means roughly
> 6 out of 10 future draws will be red.
>
> For a `sym` summary that has seen `n` values, the probability of
> drawing value `v` next is *count of v* divided by `n`. That is
> all `SYM.like` does, modulo smoothing.
>
> For a `num` summary, "the probability of `v`" needs a different
> definition because numbers are continuous. We instead ask: under
> a bell curve centred at `mu` with spread `sd`, how *dense* is the
> distribution at `v`? That is the [Gaussian
> density](#breakout-ai-gaussian-density) we use in
> [`NUM.like`](#numlike).
