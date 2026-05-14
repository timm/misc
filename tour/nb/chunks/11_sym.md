<!-- chunk: 11_sym -->
<!-- prev: 10_lec1_open  next: 11a_bo_metatables -->

### Sym

A `sym` is a column of symbolic (string-valued) values: colors,
class labels, soybean diseases. It keeps a running count of how
often each value appears, in a table called `has`. The slot `n` is
the total count of non-missing values seen so far. The slots `at`
and `txt` remember which column this is (its zero-based position
and its header name) so later code can find it again.

That is the whole structure. A `sym` is four fields:

```lua
local function Sym(n,s)
  return isa(SYM,{at=n or 0, txt=s or "", n=0, has={}}) end
```

The `isa(SYM, ...)` call attaches a method table — we explain that
in the [metatables breakout](#breakout-language-metatables) below.
For now, read it as "this table is a `SYM`".

<!-- ref:l1-start -->
```
[?]> Sym(1, "color")
SYM{:at 1 :has {} :n 0 :txt color}
```

The freshly built `sym` has no counts. We can add some values one
at a time:

```
[?]> local s = Sym(); s:add("red"); s:add("red"); s:add("blue"); s
SYM{:at 0 :has {:blue 1 :red 2} :n 3 :txt }
```

`red` shows up twice, `blue` once, `n=3`. The `eg["--sym"]` demo
does the same thing using a helper called [`adds`](#adds) we will
meet in two stanzas:

```
[?]> adds({"a","a","a","b","c"}, Sym())  -- == eg["--sym"]
SYM{:at 0 :has {:a 3 :b 1 :c 1} :n 5 :txt }
```

That same pattern — declare a structure, then teach it `add` —
repeats for [`Num`](#num) below.
