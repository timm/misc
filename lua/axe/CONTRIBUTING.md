# Contributing

## Install


## Coding Patterns

- Everything in locals
  - So no rewrite of built-in functions
- All files end in `return {a=a, b=b}` etc
  offering points to the 
  externally useful locals objects.
- Max line length = 70
- `function X.y()` is a class method (e.g. X.new()
- `function X:y()` is an instance method.
- For religious reasons, I use polymorphism but not inheritance.
- Minimize function length
  - Use one liners, when you can.
  - Move all lines with just `end` to end of line above
- All tests are separate in `tests.lua` (which
  runs all tests via `lua tests.lua`).
  - That file should start with `math.randomseed(1)`
    - and end with a call to `Lib.rogues()` (to 
      look for escaped locals)

