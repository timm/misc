<!-- chunk: 12a_bo_meansd -->
<!-- prev: 12_num  next: 12b_bo_welford -->

### Breakout (AI): Mean and standard deviation

> Two numbers summarize a column of numbers: the *mean* (where the
> middle is) and the *standard deviation* (how far typical values
> stray from the middle).
>
> Mean of `n` values: add them up, divide by `n`. Written `mu` in
> the code (Greek letter μ).
>
> Variance is the average squared distance from the mean.
> If your values are `v1, v2, ..., vn`, then variance is
> `((v1-mu)^2 + (v2-mu)^2 + ... + (vn-mu)^2) / (n-1)`.
> Standard deviation `sd` is the square root of variance.
>
> Squared distance, not absolute distance, because the math is
> cleaner and big outliers count more than small ones — which is
> usually what you want.
