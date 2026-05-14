<!-- chunk: 90_appendix_lua -->
<!-- prev: 27_lec2_exercises  next: 99_examples_index -->

## Appendix L. Lua reference (the bits we used)

Quick reference for the Lua constructs `nb.lua` and `lib.lua`
actually use. Skim the table; the breakouts in the body cover the
non-obvious cases.

### isa

The metatable installer from `lib.lua`. See the
[metatables breakout](#breakout-language-metatables).

```lua
local function isa(mt,t) mt.__index=mt; return setmetatable(t,mt) end
```

`mt.__index = mt` is the Lua idiom that turns `mt` into both a
class and its own method table.

### the

`the` is the global config table. Three fields, parsed from the
help string by [Main](#main): `k`, `m`, `wait`. Code that reads
`the.k` is reading what the help text declares as the default.

### lib

`lib.lua` is the utility module. `nb.lua` opens with:

```lua
local l = require"lib"
local o,isa,iter,csv,sel,BIG = l.o,l.isa,l.iter,l.csv,l.sel,l.BIG
```

The helpers used in the body:

| symbol      | one-line role |
|-------------|---------------|
| `iter(t)`   | iterator over a table or a function |
| `csv(file)` | iterator over CSV rows (each row a list) |
| `sum(t,f)`  | sum of `f(v)` over `t` |
| `kap(t,f)`  | map `f(k,v)` over `t`, return a list |
| `sel(t,f)`  | filter `t` by predicate `f` |
| `most(t,f)` | key in `t` where `f(k,v)` is largest |
| `cast(s)`   | parse a string into int / float / trimmed string |
| `o(t)`      | pretty-print any table |
| `BIG`       | `1e32` — finite stand-in for "infinity" |

### Tables as both arrays and hashes

A Lua table can hold integer-keyed entries (used as an array, with
`#t` giving the length) and string-keyed entries (used as a hash)
in the same value. `nb.lua` uses both: `i.rows` is array-like,
`i.has` is hash-like, and `i.cols` holds both.

### `nil` is missing-or-false

Lua's `nil` plays two roles: "this key has no value" and "this
condition is false". Hence `i.has[v] or 0` returns 0 when `v` is
unseen.

### Multiple return values

Lua functions may return many values. We use this once, when
`l.csv` returns the rows of a CSV — each row is itself a list of
parsed values. The `for row in csv(file) do ... end` loop reads
one row at a time.
