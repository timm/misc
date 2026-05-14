<!-- chunk: 12b_bo_welford -->
<!-- prev: 12a_bo_meansd  next: 13_col -->

### Breakout (AI): Welford's algorithm

> A textbook variance formula needs all the values at once. We do
> not have that. We see one value at a time and want the variance
> *so far*.
>
> Welford (1962) gives a recipe. Keep two running numbers: `mu`,
> the running mean, and `m2`, the running sum of squared distances
> from the running mean. On each new value `v`:
>
> 1. `n  := n + 1`
> 2. `d  := v - mu`               (distance from old mean)
> 3. `mu := mu + d / n`           (slide the mean toward v)
> 4. `m2 := m2 + d * (v - mu)`    (note: uses the *new* mu)
>
> Standard deviation falls out as `sqrt(m2 / (n-1))`.
>
> That is exactly the three working lines of [`NUM.add`](#numadd).
> No list of seen values is retained. The whole column lives in
> three floats no matter how long the file.
