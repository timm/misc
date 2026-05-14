<!-- chunk: 23a_bo_logprob -->
<!-- prev: 23_data_likes  next: 23b_bo_independence -->

### Breakout (AI): Log-probabilities

> Probabilities are small. Multiply ten of them and you get a
> number around 10^-10. Multiply a hundred and you underflow to
> zero on every standard floating-point machine.
>
> The fix is to take logarithms. The log of a product is the sum
> of the logs:
>
> ```
>   log(a * b * c) = log(a) + log(b) + log(c)
> ```
>
> So instead of multiplying probabilities, we add their logs. The
> answer is large and negative (log of a small number is negative)
> but never underflows. `nb.lua` calls `math.log` on every term and
> uses `+` instead of `*`. The class with the largest sum wins,
> which is the same class that would have had the largest product.
