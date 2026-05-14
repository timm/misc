<!-- chunk: 23_data_likes -->
<!-- prev: 22a_bo_gaussian  next: 23a_bo_logprob -->

### DATA.likes

For a single `data` (one class's summary) and one new row, what is
the joint likelihood that this row came from this class?

If column likelihoods were independent — they are not, but pretend
— the joint probability of the row is the product of the per-column
likelihoods, times the class prior. We multiply many small numbers,
so we take logs and add them instead. See the
[log-probability breakout](#breakout-ai-log-probabilities).

```lua
function DATA.likes(i,row,nall,nh,    b4)
  b4 = (#i.rows + the.m)/(nall + the.m*nh)
  return log(b4) + l.sum(i.cols.x, function(c)
    return row[c.at]~="?" and log(c:like(row[c.at],b4)) or 0 end) end
```

Read it from the inside. `c:like(row[c.at], b4)` is either
[`SYM.like`](#symlike) or [`NUM.like`](#numlike), dispatched by
column type. The `b4` argument is the *class prior* — how common
this class is in the training data so far, smoothed with `the.m`.

`l.sum(i.cols.x, f)` adds up `f(c)` over the input columns,
skipping missing values (`row[c.at] == "?"` contributes `0`,
which is the log of `1` — a multiplicative no-op). The leading
`log(b4)` is the class prior contribution.

The result is one number per class. Whichever class scores highest
is the prediction.

```
[?]> local d = Data("", lib.csv("data/diabetes.csv"))
[?]> d:likes(d.rows[1], #d.rows, 2)  -- == eg["--likes"]
-28.309945819742
```

`-28` is a log-likelihood. The number is meaningless on its own;
its meaning is "this row scores -28 against the all-data summary".
What matters is comparison across classes — see [`nb`](#nb).
