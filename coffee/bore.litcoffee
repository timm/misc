Stuff needed from elsewhere.

    readline  = require 'readline'
    fs        = require 'fs'

My own config

    magic =
      num: '$'
      more: '>'
      less: '<'
      klass: '!'
      ignore: '?'

    data = "../data/"
    conf = 95
    
Standard constants

    ninf   = -1 * (Number.MAX_SAFE_INTEGER - 1)
    inf    = Number.MAX_SAFE_INTEGER
    tiny   = 1 / @inf

Standard functions.

    same   = (x) -> x 
    ok     = (f,t) -> throw new Error t or "" if not f
    today  = () -> Date(Date.now()).toLocaleString().slice(0,25)
    
    String::last = -> this[ this.length - 1]
    String::n    = (m=40) -> Array(m+1).join(this)
    
    say = (l...) ->
      sep=""
      w = (s) -> process.stdout.write(s)
      for x in l
        w(sep+ x)
        sep=", "
      w("\n")
 
    lines = ( file, action, done = (-> ) ) ->  
      stream = readline.createInterface
        input:    fs.createReadStream file
        output:   process.stdout
        terminal: false
      stream.on 'close',           -> done()
      stream.on 'error', ( error ) -> action error
      stream.on 'line',  ( line  ) -> action line 
    
## CSV files

    class Csv
      constructor: (file, action, over) ->
        @use     = null
        @lines    = []
        @action  = action
        lines file, @line, over or ->
      line: (s) =>
        if s
          s = s.replace /\s/g,''
          s = s.replace /#.*/,''
          @merge s if s.length
      merge: (s) ->
        @lines.push s
        if s.last() isnt ','
          @act @lines.join().split ','
          @lines = []
      act: (cells) ->
        if cells.length
          @use or= (i for c,i in cells when magic.ignore not in c)
          @action (@prep cells[i] for i in @use)
      prep: (s) ->
        t = +s
        if Number.isNaN(t) then s else t

## Columns

Storing info about a column.

    class Col
      constructor: (x,p,w=1) -> [@n,@w,@pos,@txt]=[0,w,p,x]
      norm: (x) -> if x isnt magic.ignore then @norm1 x else x
      #---------------------
      add: (x) ->
         if x isnt magic.ignore
           @n++
           @add1 x
         x
      #---------------------
      adds: (a,f=same) ->
         (@add f(x) for x in a)
         this
    
Storing info about numeric columns.

    class Num extends Col
      constructor: (args...) ->
        super args...
        [ @mu,@m2,@sd ] = [ 0,0,0,0 ]
        [ @hi, @lo ]    = [ ninf, inf ]
      #-------------------------
      mid:    () -> @mu
      var:    () -> @sd
      norm1: (x) -> (x - @lo) / (@hi - @lo +  _.tiny)
      toString:  -> " #{@n}:#{@lo}..#{@hi}"
      #-------------------------
      add1: (x) ->
        @lo = if x < @lo then x else @lo
        @hi = if x > @hi then x else @hi
        delta = x - @mu
        @mu += delta / @n
        @m2 += delta * (x - @mu)
        @sd  = @sd0()
      #-------------------------
      sd0: () -> switch
        when  @n < 2  then 0
        when  @m2 < 0 then 0
        else (@m2 / (@n - 1))**0.5
    
Storing info about symbolic  columns.

    class Sym extends Col
      constructor: (args...) ->
        super args...
        [ @counts,@most,@mode,@_ent ] = [ [],0,null,null ]
      #------------------------
      mid:    () -> @mode
      var:    () -> @ent()
      norm1: (x) -> x
      #-------------------------
      add1: (x) ->
        @_ent = null
        @counts[x] = 0 unless @counts[x]
        n = ++@counts[x]
        [ @most,@mode ] = [ n,x ] if n > @most
      #-------------------------
      ent: (e=0)->
        if @_ent == null
          for x,y of @counts
            p  = y/@n
            e -= p*Math.log2(p)
          @_ent = e
        @_ent
    
## Tests

    okLines = (f= data + 'weather3.csv') -> lines f,say 
    
    okCsv1 = (f = data + 'weather3.csv',n=0) ->
      new Csv f, (-> ++n), (->  say "rows: " + n)
    
    okCsv2 = (f = data + 'weather3.csv',n=0) ->
      new Csv f, ((row)-> say row)
    
    okNum1 = ->
      n = new Num
      (n.add x for x in [9,2,5,4,12,7,8,11,9,3,7,4,12,5,4,10,9,6,9,4])
      say n.mu
      ok n.mu==7
     
    okNum2 = ->
      say 1
      n = new Num
      say 2
      n.adds [9,2,5,4,12,7,8,11,9,3,7,4,12,5,4,10,9,6,9,4], (x) -> 0.1*x
      say  "eg1",n.mu,n.sd,n.sd.toFixed(3)
      ok n.mu==0.7
    
    okSym = ->
      s= new Sym
      s.adds ['a','b','b','c','c','c','c']
      console.log "egSym:",s.n, s.counts, s.ent().toFixed(3)
      ok s.ent().toFixed(3) == '1.379'
    
    say ("-".n())+"\n"+today()
    ###
    okNum1()
    okNum2()
    okSym()
    okLines()
    okLines data+'weather2.csv'
    okCsv1()
    ###
    okCsv1 data+'weather3.csv'
    okCsv2 data+'weather3.csv'
    
