@ninf   = -1 * (Number.MAX_SAFE_INTEGER - 1)
@inf    = Number.MAX_SAFE_INTEGER
@tiny   = 1 / @inf
@ignore = '?'
@conf   = 95
@data   = process.env["Koffee"] + "/data" or "../data"

@say = (l...) ->
  sep=" "
  w = (s) -> process.stdout.write(s)
  for x in l
    w(sep+ x)
    sep=", "
  w("\n")

String::last = ->
  this[ this.length - 1]
