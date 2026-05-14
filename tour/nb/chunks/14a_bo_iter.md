<!-- chunk: 14a_bo_iter -->
<!-- prev: 14_adds  next: 14b_bo_colon -->

### Breakout (Language): Iterators

> Lua's `for v in EXPR do ... end` is the generic for-loop. `EXPR`
> must be a *stateful function* — call it with no arguments and
> each call returns the next value (or `nil` when exhausted).
>
> Tables are not iterators. To iterate `{2, 4, 8}`, you ask Lua's
> built-in `pairs` for an iterator. The helper `lib.iter(t)` wraps
> that so the same syntax works whether `t` is a table or already
> a function:
>
> ```lua
> local function iter(t,    more,state,key)
>   if type(t)=="function" then return t end
>   more,state,key = pairs(t)
>   return function(v) key,v = more(state,key); return v end end
> ```
>
> If `t` is already an iterator, hand it back. Otherwise wrap
> `pairs(t)` so successive calls return successive values.
>
> Same idea as Python's generators or Java's `Iterable`. Lua just
> exposes the protocol directly instead of hiding it behind a class.
