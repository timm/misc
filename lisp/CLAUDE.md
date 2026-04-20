# Coding style (reverse-engineered from ezr.lua, ezr.lisp, ezr.jl)

This project ports the same active-learning algorithm across Lua, Common Lisp,
and Julia. Match the style already on disk — it is deliberate and consistent.

## Principles

- **SSOT.** One place defines a thing. Config lives only inside the help
  docstring and is parsed by regex; do not mirror it into a struct. Column
  roles are inferred from header suffixes, not re-declared elsewhere.
- **Part-of, not is-a.** Favor composition. `DATA` has `COLS`; `COLS` has
  `NUM`s and `SYM`s; `TREE` has left/right `TREE`. No inheritance chains.
  Metatables are used for method dispatch, not for `extends`.
- **Uncle Bob sized functions.** Most functions fit on a screen; many on a
  single line. One function, one job. If it grows, split it (see how
  `validate` delegates to `active`, `Tree:build`, `Tree:leaf`).
- **DRY via a small library.** Shared helpers (`push`, `map`, `sort`, `sum`,
  `kv`, `slice`, `shuffle`, `csv`, `o`, `fmt`, `crop`, `thing`, `new`) live
  in one namespace (`l` in Lua). Don't re-roll them inline.
- **YAGNI.** No error handling, fallbacks, or validation beyond the I/O
  boundary (CSV open, flag parse). No hypothetical generality.
- **Pure where possible.** Higher-order helpers (`map`, `sum`, `sort`, `kv`)
  take a function. Pass closures inline; don't name one-use lambdas.
- **Guard-clause early returns.** Handle the degenerate case, bail, then
  write the main body unindented.
- **Minimise state.** Cache derived values with a `_`-prefixed field
  (`_mid`); invalidate by setting to nil on write.

## Config / CLI

- `key=default` syntax is **reserved** for "setting = default value" inside
  the help docstring (e.g. `-B Budget=50`). Do not use `x=y` prose anywhere
  else in docs or comments — it signals a settable flag.
- Flags are one short letter (`-s`, `-p`) or a `--word` command. Commands
  live in an `eg` table keyed by the flag string.
- `--all` runs every `--xxx` demo, reseeding before each.

## Layout

- Two-space indent. No tabs.
- Section banners: `-- ## name --------` (pad with hyphens, ~60 cols total).
- File header: shebang, one-line purpose, `(c) YEAR Tim Menzies ...`, then
  USAGE / CMDS / OPTIONS / CSV INPUT / NAMING inside a single docstring.
- One file per language port. Don't split into many modules.

## Naming

Single-letter variables are the norm and are documented in the header's
NAMING block. Keep the convention:

- `i` = self (in methods); `c` = column; `r`, `rs` = row, rows; `d` = Data;
  `t` = table/tree node; `x`/`y` = feature/goal column; `fn` = function;
  `txt` = string; `j` = iterator; `_mid` = cached centroid.
- Types are ALL-CAPS: `NUM`, `SYM`, `COLS`, `DATA`, `TREE`.
- Constructors are Capitalized: `Num`, `Sym`, `Data`, `Tree`.
- Utility library is `l.*`; demos are `eg["--name"]`.
- `self` is always `i`, never `self` or `this`.

## Comments

- Sparse (~3% of lines). Only comment the *why* when non-obvious
  (e.g. "Sigmoid-of-z-score approximates the Gaussian CDF in [0,1]").
- No docstrings, no type hints, no multi-paragraph blocks.
- Don't restate what code does; don't reference tickets, callers, or the
  task that prompted the change.

## Idioms

- `and`/`or` ternary (`cond and a or b`) over if/else for simple branches.
- Chain tight statements with `;` on one line (`t={}; id=id+1; t.id=id`).
- Early-return guards (`if not i.col then return i end`).
- Missing values are the literal string `"?"`.
- Sentinel infinities: `1E32` / `1e-32`. Don't reach for `math.huge`.

## Lua-specific

- Forward-declare locals in a cluster at the top so mutually recursive
  functions can see each other (ezr.lua:42–49).
- Alias stdlib into locals: `local abs,min,max = math.abs,math.min,math.max`.
- Module ends with an explicit `return {...}` exporting only what callers
  need; everything else stays `local`.
- 4-space gap in signatures separates params from scratch locals:
  `function Cols(names,    xs,ys,all,col)`.
- Methods: `CLASS.method = function(i,...)`; dispatch with `:` syntax.
- No space after `:match` / `:gmatch` on string literals:
  `s:match"^[A-Z]"`, not `s:match("^[A-Z]")`.
- One-liner function bodies keep `end` on the last work line.
- `//` integer division; `#t` length; `l.push(t,x)` to append.
- CSV reader returns an iterator closure that yields typed rows.

## Don'ts

- Don't add dependencies. Lua uses only `math`/`string`/`table`/`io`.
- Don't rename single-letter vars to "descriptive" names.
- Don't expand one-liners into multi-line blocks for readability.
- Don't add error handling or validation beyond I/O boundaries.
- Don't introduce inheritance or class hierarchies.
- Don't duplicate config (flags, defaults, column-role rules).
