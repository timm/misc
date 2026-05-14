<!-- chunk: 14_adds -->
<!-- prev: 13_col  next: 14a_bo_iter -->

### adds

`adds` is the bulk loader. Give it any sequence of values (a Lua
table, or another iterator) and a target, and it walks the sequence
calling `target:add(v)` on each.

```lua
local function adds(items,t)
  t=t or Num();for v in iter(items or {})do t:add(v) end; return t end
```

Three small ideas in one line.

1. If you do not pass a target, you get a fresh [`Num`](#num).
   That makes `adds({10,20,30,40})` a one-liner for "give me the
   summary of these numbers".
2. `iter(items)` turns its argument into something you can `for ...
   in` over. The [iter breakout](#breakout-language-iterators)
   covers how that works. Worth reading once.
3. The colon in `t:add(v)` is Lua's method-call sugar — see the
   [colon breakout](#breakout-language-method-calls).

```
[?]> adds({10,20,30,40,50})
NUM{:at 0 :m2 1000.00 :mu 30.00 :n 5 :sd 15.81 :txt }
```

`adds` is doing two jobs that the body splits across the file: it
is the *bulk add* primitive (used everywhere a fresh column wants
filling), and it is the loader hidden inside [`Data`](#data) below.
That dual use is the reason it exists at all.
