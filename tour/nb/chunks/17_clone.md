<!-- chunk: 17_clone -->
<!-- prev: 16_cols  next: 18_data_add -->

### clone

`clone` makes a fresh empty `data` with the *same column structure*
as another. It is a copy of the header, not the rows.

```lua
function clone(data,rows)
  return adds(rows, Data(data.txt, {data.cols.names})) end
```

`Data(data.txt, {data.cols.names})` constructs a new `data` and
hands it a one-row list whose only entry is the original header.
That triggers [`DATA.add`](#dataadd)'s "first row is header" branch,
so the clone gets fresh `Sym`s and `Num`s with the original names
and positions but zero counts. Then `adds(rows, ...)` optionally
pours in some rows.

We will use `clone` once, in [`nb`](#nb), to keep one `data` per
class label as we go. Each per-class clone summarizes "what does
this class look like?" — the table of those summaries is the
classifier.
