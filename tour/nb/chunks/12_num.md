<!-- chunk: 12_num -->
<!-- prev: 11a_bo_metatables  next: 12a_bo_meansd -->

### Num

A `num` is a column of numeric values: ages, weights, blood-pressure
readings. Unlike a `sym`, we do not keep every value we have seen.
We keep just enough to recover the *mean* and *standard deviation*
at any moment. That is six fields:

```lua
local function Num(n,s)
  return isa(NUM,{at=n or 0, txt=s or "", n=0, mu=0, m2=0, sd=0}) end
```

`mu` is the running mean. `m2` is a helper for variance (see the
[Welford breakout](#breakout-ai-welfords-algorithm)). `sd` is the
running standard deviation, recomputed on every `add`. `n` is the
count of non-missing values, same as in `sym`.

```
[?]> Num(3, "Age")
NUM{:at 3 :m2 0 :mu 0 :n 0 :sd 0 :txt Age}
```

Add four numbers and read the summary back:

```
[?]> adds({10,20,30,40})       -- == eg["--num"]
NUM{:at 0 :m2 500.00 :mu 25.00 :n 4 :sd 12.91 :txt }
```

Mean 25, standard deviation 12.91, and `m2` (the sum of squared
deviations from the running mean) is 500. Three numbers tell you
the centre and the spread of a column you may never store in full.
