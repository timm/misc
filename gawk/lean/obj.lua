--<!-- vim: set syntax=lua ts=2 sw=2 et : -->
local lib   = require"lib"
local stats = require"stats"
local l={}

-- ## One Col ---------------------------------------------
function l.COL(n,s) --> NUM or SYM
  return ((s or ""):find"^[A-Z]" and l.NUM or l.SYM)(n,s) end

function l.SYM(n,s) --> SYM
  return {at=n, txt=s, n=0, isIgnored = (s or ""):find"X$",
          has={}, most=0, mode=nil,
          isSym=true, } end

function l.NUM(n,s) --> NUM
  return {at=n, txt=s, n=0, isIgnored = (s or ""):find"X$",
          mu=0, m2=0, sd=0,
          heaven = (s or ""):find"-$" and 0 or 1} end

function l.col(col1, any, t, sym, num)  --> nil
  function sym(t)
    t[any] = 1 + (t[any] or 0)
    if t[any] > col1.most then
      col1.most, col1.mode = t[any], any end 
  end -----------------
  function num(d)
    -- datagenetics.com/blog/november22017/index.html xplains this:
    d       = any - col1.mu
    col1.mu = col1.mu + d/col1.n
    col1.m2 = col1.m2 + d*(any - col1.mu) 
    if col1.n>1 then col1.sd =  (col1.m2/(col1.n-1))^.5 end 
  end ---------------
  if any ~= "?" then
     col1.n = col1.n + 1
     if col1.isSym then sym(col1.has) else num() end end end

function l.mid(col1) --> any
  return col1.isSym and col1.mode or col1.mu end

function l.div(col1) --> n
  return col1.isSym and stats.entropy(col1.has) or col1.sd end

-- ## Many Cols ---------------------------------------------
function l.COLS(t,      x,y,all,klass) --> COLS
  x, y, all = {}, {}, lib.kap(t, l.COL) 
  for at, col1 in pairs(all) do
    if not col1.isIgnored then
      if col1.txt:find"!$" then klass=col1 end
      (col1.txt:find "[-!+]$" and y or x)[at]=col1 end end
  return {klass=klass, names=t, x=x, y=y, all=all} end

function l.cols(cols1, t) --> nil
  for _, col1 in pairs(cols1.all) do
    l.col(col1, t[col1.at]) end end

-- ## Cols and Rows ---------------------------------------------
function l.DATA(s_or_ts,    data1) --> DATA
  data1 = {rows={},cols=nil}
  if   type(s_or_ts)=="string"
	then for _,t in lib.csv(s_or_ts) do l.data(data1,t) end
	else for _,t in pairs(s_or_ts)   do l.data(data1,t) end end
	return data1 end

function l.data(data1, t) --> nil
  if   data1.cols
  then l.cols(data1.cols, t)
       data1.rows[ 1+#data1.rows ] = t
  else data1.cols = l.COLS(t) end end

function l.stats(data1, cols1,fun,ndecs,     t) --> t[s=n]
    t = { N = #data1.rows }
    for _, col1 in pairs(data1.cols[cols1 or "y"]) do
        t[col1.txt] = lib.rnd((fun or l.mid)(col1), ndecs) end
    return t end

function l.clone(data1,  ts,     data2) --> DATA
  data2 = l.DATA({data1.cols.names})
  for _,t in pairs(ts or {}) do l.data(data2,t) end 
  return data2 end

--------------------------------------------------------
return l
