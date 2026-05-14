<!-- chunk: 11a_bo_metatables -->
<!-- prev: 11_sym  next: 12_num -->

### Breakout (Language): metatables

> Lua has no `class` keyword. Object behavior is glued on with
> *metatables*. A metatable is just another table; when you look up
> a field on a regular table and miss, Lua falls back to the
> metatable's `__index` field. So if you set `SYM.__index = SYM`
> and put `add` and `like` directly on `SYM`, then any table whose
> metatable is `SYM` can call `s:add(v)` and find `add` via the
> miss-and-fall-through. The helper [`isa`](#isa) in `lib.lua`
> wires this up:
>
> ```lua
> local function isa(mt,t) mt.__index=mt; return setmetatable(t,mt) end
> ```
>
> So `isa(SYM, {at=...})` produces a plain table tagged as a `SYM`.
> No inheritance, no constructors, no `new`. Just look-up
> fall-through.
