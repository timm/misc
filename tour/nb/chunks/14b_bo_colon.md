<!-- chunk: 14b_bo_colon -->
<!-- prev: 14a_bo_iter  next: 15_data -->

### Breakout (Language): Method calls

> In Lua, `t:add(v)` is exactly equivalent to `t.add(t, v)`. The
> colon is sugar that inserts `t` as the first argument so the
> method can refer to it. Inside the function definition, the
> matching sugar is:
>
> ```lua
> function SYM.add(i, v)   -- written with a dot, takes `i` explicitly
> ```
>
> versus
>
> ```lua
> function SYM:add(v)      -- written with a colon, `self` is implicit
> ```
>
> `nb.lua` uses the dot form and names the receiver `i` (for
> *instance*). The first form is more honest: you can see what is
> being passed.
