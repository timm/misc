# Coding Patterns

- Everything in locals
  - So no rewrite of built-in functions
- All files end in `return {a=a, b=b}` etc
  offering points to the 
  externally useful locals objects.
- Max line length = 70
  - So use `i`, not `self` for self-reference
  - So do not use ":" (which demands we use `self`). 
- Create classes by cascaded calls to `thing.new()`
  - Create instances classes via `local xx = {ako="xx"}`
  - And in constructors, set `i.Is` to a pointer
    to the class.
- Minimize function length
  - Use one liners, when you can.
  - Move all lines with just `end` to end of line above
- All tests are separate in `tests.lua` (which
  runs all tests via `lua tests.lua`).
  - That file should start with `math.randomseed(1)`
    - and end with a call to `lib.rogues()` (to 
      look for escaped locals)

