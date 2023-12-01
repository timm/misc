local lib   = require"lib"
local stats = require"stats"
local l={}

-- ## One Col ---------------------------------------------
function l.COL(n,s)
  return ((s or ""):find"^[A-Z]" and l.NUM or l.SYM)(n,s) end

function l.SYM(n,s)
  return {at=n, txt=s, isIgnored = (s or ""):find"X$",
          isSym=true, has={},ok=false } end

function l.NUM(n,s) 
  return {at=n, txt=s, isIgnored = (s or ""):find"X$",
          has={}, isSorted=false,
          heaven = (s or ""):find"-$" and 0 or 1} end

function l.col(col1,any,     t) 
  if any ~= "?" then
    t = col1.has 
    if   col1.isSym
    then t[any] = 1+(t[any] or 0) 
    else t[1+#t]=any; col1.isSorted=false end end end

function l.has(col1)
  if not (col1.isSym or col1.isSorted) then table.sort(col1.has); col1.isSorted=true end
  return col1.has end

function l.mid(col1)
  return (col1.isSym and stats.mode or stats.median)(l.has(col1)) end

function l.div(col1)
  return (col1.isSym and stats.entropy or stats.spread)(l.has(col1)) end

-- ## Many Cols ---------------------------------------------
function l.COLS(t,      x,y,all)
  x, y, all = {}, {}, lib.kap(t, l.COL) 
  for at, col1 in pairs(all) do
    if not col1.isIgnored then
      (col1.txt:find "[-!+]$" and y or x)[at]=col1 end end
  return {names=t, x=x, y=y, all=all} end

function l.cols(cols1, t)
  for _, col1 in pairs(cols1.all) do
    l.col(col1, t[col1.at]) end end

-- ## Cols and Rows ---------------------------------------------
function l.DATA(s_or_ts,    data1)
  data1 = {rows={},cols=nil}
  if   type(s_or_ts)=="string"
	then for _,t in lib.csv(s_or_ts) do l.data(data1,t) end
	else for _,t in pairs(s_or_ts)   do l.data(data1,t) end end
	return data1 end

function l.data(data1, t)
  if   data1.cols
  then l.cols(data1.cols, t)
       data1.rows[ 1+#data1.rows ] = t
  else data1.cols = l.COLS(t) end end

function l.stats(data1, fun,ndecs,cols1,     t)
    t = { N = #data1.rows }
    for _, col1 in pairs(cols1 or data1.cols.y) do
        t[col1.txt] = lib.rnd((fun or l.mid)(col1), ndecs) end
    return t end

function l.clone(data1,  ts,     data2)
  data2 = DATA({data1.col.names})
  for _,t in pairs(ts or {}) do  data(data2,t) end 
  return data2 end

return l
