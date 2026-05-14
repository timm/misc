<!-- chunk: 26_main -->
<!-- prev: 25_eg  next: 27_lec2_exercises -->

### Main

The last block of the file does two jobs.

```lua
for k,v in help:gmatch("(%S+)=(%S+)") do the[k]=l.cast(v) end
if arg[0] and arg[0]:find"nb" then
  for j,s in pairs(arg) do if eg[s] then eg[s](arg[j+1]) end end end
```

First, fill in the [`the`](#the) settings table by parsing the
`help` string at the top of the file. Every `key=value` token in
the help text is parsed and stored. That is why a line like
`-k k=1` in the OPTIONS section is both human-readable
documentation *and* the canonical default for `the.k`. Editing
documentation edits the program.

Second, if this file was launched directly (the script name on the
command line contains `nb`), walk the command-line arguments and
fire the matching `eg` for each `--flag` seen. The argument *after*
the flag is passed to the function — that is how `--data
data/diabetes.csv` gets the filename through.

<!-- ref:l2-end -->
```
[?]> lua nb.lua --the
{:k 1 :m 2 :wait 5}
```

Three knobs: `k` (smoothing for sym likelihoods), `m` (smoothing
for the class prior), `wait` (burn-in rows before classification
starts). All set from the help text.
