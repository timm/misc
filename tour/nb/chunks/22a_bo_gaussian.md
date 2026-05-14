<!-- chunk: 22a_bo_gaussian -->
<!-- prev: 22_num_like  next: 23_data_likes -->

### Breakout (AI): Gaussian density

> The Gaussian (or normal, or bell-curve) density at `v` for mean
> `mu` and variance `var` is:
>
> ```
>            1               -(v - mu)^2
>   ----------------  *  exp ----------
>     sqrt(2*pi*var)            2*var
> ```
>
> The shape is a hill centred at `mu`, with width controlled by
> `sd = sqrt(var)`. Values close to the mean get a high density;
> values far away get an exponentially small one. The `1/sqrt(...)`
> factor scales the area under the hill to exactly 1.
>
> For naive Bayes you do not need to remember the constant. What
> matters is "values near the mean are likely; values far from the
> mean are unlikely, very fast."
