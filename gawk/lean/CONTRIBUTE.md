# Contribute

## Coding standards
- Try to keep functions 5 lines long, or less
- Avoid globals. Module locals are kept inside `l`.
- Constructor functions are written in UPPER CASE; e.g. DATA
  - Functions that update constructed instances are the same name, but in lower case
  - so the `data` function updates `DATA` instances

### Type hints
- Constructor names are used for variable names as type hint
  - But don't use the raw name, add (e.g.) a number
  - For example `data1` is an instance of type `DATA`.
- `t` denotes a table of anything at all
- `n` is usually for a number
- `isX` is for booleans

### Function arguments
- Function arguments proceeded by two spaces are optional.
- Function arguments proceeded by four spaces are local variables

## Common idioms
- `t[ 1 + #t ]=x` means push `x` to the end of the list (fyi: `#t` returns the size of the list `t`)
