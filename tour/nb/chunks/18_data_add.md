<!-- chunk: 18_data_add -->
<!-- prev: 17_clone  next: 19_lec1_exercises -->

### DATA.add

The `add` method for `DATA` is two cases stacked together:

```lua
function DATA.add(i,row)
  if not i.cols then i.cols=Cols(row) else
    i.rows[1+#i.rows] = row
    for _,col in pairs(i.cols.all) do col:add(row[col.at]) end end end
```

If `i.cols` is `nil`, the incoming row is the header. Build a
[`Cols`](#cols) from it and we are done with this row. Otherwise
the row is data: append it to `i.rows`, then push each value
through the matching column's `add`.

The sibling adds for individual columns are the obvious pair —
count for symbols, [Welford](#breakout-ai-welfords-algorithm) for
numbers. Both ignore missing values, marked `"?"`.

```lua
function SYM.add(i,v)
  if v~="?" then i.n=i.n+1; i.has[v]=1+(i.has[v] or 0) end end

function NUM.add(i,v,    d)
  if v~="?" then
    i.n=i.n+1; d=v-i.mu; i.mu=i.mu+d/i.n; i.m2=i.m2+d*(v-i.mu)
    i.sd = i.n<2 and 0 or sqrt(i.m2/(i.n-1)) end end
```

This is the rhyme we promised. Same shape on both sides: skip
missing, bump `n`, update the summary. The two functions even line
up vertically. That symmetry is the teaching point — *one* concept
("incrementally summarize"), *two* types ("sym" and "num").

We are done with the data engineering. Everything from here forward
is "now that the data summarizes itself, what can we ask it?"
