<!-- chunk: 16_cols -->
<!-- prev: 15_data  next: 17_clone -->

### Cols

`Cols` builds the per-column summary table from a header row. Three
slots:

- `names` — the raw header strings, kept for round-tripping.
- `all` — every column as a [`Sym`](#sym) or [`Num`](#num).
- `x` — the *input* columns: every column that is neither the
  class label nor explicitly ignored.
- `y` — the *output* columns: every column whose name ends in `!`.

The split is done by a regex on the suffix:

```lua
function Cols(row,    all)
  all = l.kap(row, Col)
  return isa(COLS, {names=row, all=all,
    x = sel(all, function(c) return not c.txt:find"[!X]$" end),
    y = sel(all, function(c) return c.txt:find"!$" end)}) end
```

`l.kap(row, Col)` maps `Col(n, header)` over each `(n, header)`
pair — that is `Cols`'s contribution to the column-type dispatch.
`sel(all, predicate)` filters the result by suffix.

```
[?]> Cols({"Name","Age","Weight-","Class!"}).y   -- == eg["--cols"]
{NUM{:at 4 :m2 0 :mu 0 :n 0 :sd 0 :txt Class!}}
```

The header `Class!` ends in `!` and starts with an uppercase letter
— so it is numeric AND a class label. `nb.lua` does not separate
those concerns. (The diabetes dataset uses lowercase `class!`,
which is symbolic and a label — the more common case.)

The naming `x` and `y` is convention from regression and
classification literature: the inputs predict the output.
