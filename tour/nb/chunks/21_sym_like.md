<!-- chunk: 21_sym_like -->
<!-- prev: 20_lec2_open  next: 21a_bo_prob -->

### SYM.like

Given a `sym` summarizing one class's values for a column, and a
new value `v`, *how likely is `v` under this summary?*

For a fair coin observed flipping H-H-T-H, the chance of the next
flip being H is about 3/4. That fraction — count of matches over
total count — is the answer. Plus one small twist: we add a tiny
*smoothing* term so a never-before-seen value gets a non-zero
probability instead of a flat zero. The twist is called the
[m-estimate](#breakout-ai-the-m-estimate).

<!-- ref:l2-start -->
```lua
function SYM.like(i,v,prior,    n)
  n = (i.has[v] or 0) + the.k*(prior or 0)
  return max(1/BIG, n/(i.n + the.k + 1/BIG)) end
```

`i.has[v] or 0` is the count of `v` in this class — zero if
unseen. `the.k * prior` is the smoothing nudge: pretend you have
seen `k` extra fake observations distributed according to a prior.
Then divide by the total count plus the same smoothing mass.
`max(1/BIG, ...)` floors the result at a tiny positive number; we
never return zero, because a single zero will sink the whole
product in [`DATA.likes`](#datalikes).

```
[?]> local s = adds({"a","a","a","b","c"}, Sym()); s:like("a", 0.5)
0.58333333333333
```

`a` shows up three times out of five. With `k=1` and `prior=0.5`,
`SYM.like` returns `(3 + 0.5) / (5 + 1) = 0.5833...`. The prior
nudges the raw fraction `3/5 = 0.6` down a little; the smaller
your evidence, the more the prior matters.
