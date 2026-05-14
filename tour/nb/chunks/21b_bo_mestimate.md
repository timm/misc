<!-- chunk: 21b_bo_mestimate -->
<!-- prev: 21a_bo_prob  next: 22_num_like -->

### Breakout (AI): The m-estimate

> If you have seen four red marbles in four draws, the raw
> probability of red is 4/4 = 1.0 and of blue is 0/4 = 0.0. That
> says "blue is impossible". A single small sample should not let
> you make that claim.
>
> The *m-estimate* fixes it. Pretend you saw `m` extra imaginary
> draws distributed according to a prior belief `p`. Your new
> estimate is `(count + m*p) / (n + m)`.
>
> In `nb.lua`, `the.k = 1` is the count of imaginary draws for
> [`SYM.like`](#symlike), and `the.m = 2` is the count for the
> *class prior* inside [`DATA.likes`](#datalikes). Two different
> knobs, same idea. Both default to small positive integers — just
> enough to soften zeros, not enough to swamp real data.
