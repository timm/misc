<!-- chunk: 13_col -->
<!-- prev: 12b_bo_welford  next: 14_adds -->

### Col

`Col` is the factory that picks between [`Sym`](#sym) and
[`Num`](#num) based on the column header. The convention is:
**uppercase initial means numeric**, lowercase means symbolic. So
`Age` and `MASS` are numeric, `name` and `class!` are symbolic.

```lua
function Col(n,s) return (s:find"^[A-Z]" and Num or Sym)(n,s) end
```

That one line is dense, so let us unpack it. `s:find"^[A-Z]"`
returns a non-`nil` value if `s` starts with an uppercase letter.
Lua's `and` / `or` then form a ternary: pick `Num` if the test is
truthy, otherwise `Sym`. Call whichever was picked with `(n,s)`.

```
[?]> Col(1, "Age"), Col(2, "name")   -- == eg["--col"]
NUM{:at 1 :m2 0 :mu 0 :n 0 :sd 0 :txt Age}	SYM{:at 2 :has {} :n 0 :txt name}
```

This is the column-type *dispatch* idiom: one factory, two product
types, switched by a property of the input. We will see the same
shape inside [`DATA.likes`](#datalikes) when one method asks each
column to compute its own likelihood.
