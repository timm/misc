<!-- chunk: 22_num_like -->
<!-- prev: 21b_bo_mestimate  next: 22a_bo_gaussian -->

### NUM.like

The numeric mirror of [`SYM.like`](#symlike). We do not have a
count; we have `mu` and `sd`. The standard move is to assume the
column is roughly bell-shaped (Gaussian) and return the *density*
of that bell at `v`:

```lua
function NUM.like(i,v,    z,var)
  z=1/BIG; var=i.sd^2 + z
  return (1/sqrt(2*math.pi*var)) * exp(-((v - i.mu)^2)/(2*var)) end
```

`var` is variance, the square of `sd`, plus a tiny `z = 1/BIG` so
we never divide by zero when `sd` is exactly zero (which happens
before two values have been seen). The big expression is the
[Gaussian density](#breakout-ai-gaussian-density). Read its parts:

- `(v - i.mu)^2` — squared distance of the new value from the
  column mean.
- `... / (2*var)` — scale that distance by the spread.
- `exp(-...)` — convert distance into a peak-at-mean falloff: 1 at
  the mean, smaller as you walk away.
- `1/sqrt(2*pi*var)` — the normalizing constant that makes the
  bell integrate to 1. We will divide it out implicitly when we
  compare classes against each other.

```
[?]> adds({10,20,30,40,50}):like(30)
0.025231325220202
```

A `Num` summarizing `{10,20,30,40,50}` has `mu=30`, `sd≈15.81`. The
density at `v=30` (right on the mean) is about 0.025. That is not
"probability 2.5%" — it is density. Different units, but for
classification only the *relative* size matters.
