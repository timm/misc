<!-- chunk: 15_data -->
<!-- prev: 14b_bo_colon  next: 16_cols -->

### Data

A `data` is a table of rows plus a [`cols`](#cols) — the per-column
summaries. The constructor is a one-liner that just delegates to
[`adds`](#adds):

```lua
function Data(s,items)
  return adds(items or {},isa(DATA,{txt=s or "",rows={},cols=nil})) end
```

Fresh `data` has no rows and no cols. `cols` stays `nil` until the
first row arrives — that row is the header, and seeing it tells
[`DATA.add`](#dataadd) what kind of column each position is. After
the header, every subsequent row gets two things done to it: it is
appended to `rows`, and each of its values is added to the
corresponding column summary.

You will rarely build a `data` by hand. Almost every use looks
like:

```
[?]> Data("", lib.csv("data/diabetes.csv")).cols.y[1]  -- == eg["--data"]
SYM{:at 9 :has {:tested_negative 500 :tested_positive 268} :n 768 :txt class!}
```

Read the CSV, build a `data`, ask it for the first `y` (class)
column. The answer is a `sym` showing 500 negatives and 268
positives in the file. We have not done any learning yet — but we
already know the class prior.
