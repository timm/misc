# Contribute

## Coding standards
- Try to keep functions 5 lines long, or less
- Avoid globals. Module locals are kept inside `l`.
- Constructor functions are written in UPPER CASE; e.g. DATA
  - Functions that update constructed instances are the same name, but in lower case
  - so the `data` function updates `DATA` instances
- Create strings representing an  instance `x` using `lib.o(x)` (and print that string using `lib.oo(x)`).
- Control settings are parsed from a docstring at the top of file (see `lib.settings`)
  - Settings need to contain at least `help=false` and `seed=1234567891`
  - Control settings can be optionally updated from the command line (see `lib.cli` )
    - See `lib.run`.  
- Examples, tests are stored inside `eg`.  Before running an `eg` we reset the random number seed, and cache the current setttings.
  - After running an example, to undo any actions isnde the example, we reset the settings from that cache.
  - See `lib.try`. 

## Data standards
- My csv files have a row one cntaining column names
  - Names starting with uppercase are numberic (else, you are symbolic)
  - Names ending with `X` are to be ignored during the reasoning
  - Some names denote depedent variables.
    - Names ending with `+` are goals to be maximized.
    - Names ending with `-` are goals to be minimized.
    - Names ending with `!` are the class column. There should only be one of these
  - Names without `+ - !` are independent variables.

### Type hints
- Constructor names are used for variable names as type hint
  - But don't use the raw name, add (e.g.) a number
  - For example `data1` is an instance of type `DATA`.
- `t` denotes a table of anything at all
- `n` is usually for a number
- `isX` is for booleans
- `u` us usually some temporary generated from `t`.

### Function arguments
- Function arguments proceeded by two spaces are optional.
- Function arguments proceeded by four spaces are local variables

## Common idioms
- `...` is a list of all arguments from there, onwards
- `for k,v in pairs(t) do` is like Python's `for k,v in enumerate(t): `
- `t[ 1 + #t ]=x` means push `x` to the end of the list (fyi: `#t` returns the size of the list `t`)
- `yyy=require"xxx"` checks if  file `xxx.lua` has been loaded. If not, it gets loaded
  - And `yyy` gets set to whatever `xxx.lua` returns on its last line.   
- Module files start with `local l={}` and all the module details are added to `l`.
  - Module files end with `return l`; i.e. all the names in this module are available via a `require`. 
