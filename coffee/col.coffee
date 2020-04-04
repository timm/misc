@ignore = '?'

class Col
  constructor: (txt,w,pos) ->
     @n   = 0
     @w   = w or 1
     @pos = pos
     @txt = txt

  add: (x) ->
     if x isnt @ignore
       @n++
       @add1 x
     x

  adds: (a,f) ->
     f = f or (x) -> x  # the default `f` is "do nothing"
     (@add f(x) for x in a)
     this

  norm: (x) ->
     if x isnt ignore then @norm1 x else x

@Col=Col
