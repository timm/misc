local lib   = require"lib"
local stats = require"stats"
local l={}

-- ## One Col ---------------------------------------------
function l.SYM(at,s)
    return {at=at, txt=s, n=0, 
            isSym=true, has={},ok=false}  end

function l.NUM(at,s) 
  return {at=at, txt=s, n=0, 
          has={}, unsorted=false,
          heaven = (s or ""):find"-$" and 0 or 1} end

function l.COL(at,s)
  return ((s or ""):find"^[A-Z]" and l.NUM or l.SYM)(at,s) end

function l.col(col1,x,     t) 
  if x ~= "?" then
    col1.n = col1.n + 1
     t = col1.has 
    if col1.isSym
    then t[x] = 1+(t[x] or 0) 
    else t[1+#t]=x; col1.unsorted=true end end end

function l.has(col1)
  if col1.unsorted then table.sort(col1.has); col1.unsorted=false end
  return col1.has end

function l.mid(col1)
  return (col1.isSym and stats.mode or stats.median)(l.has(col1)) end

function l.div(col1)
  return (col1.isSym and stats.entropy or stats.spread)(l.has(col1)) end

-- ## Many Cols ---------------------------------------------
function l.COLS(t,      cols1)
  cols1= {x={}, y={}, names=t, all=lib.kap(t, l.COL)} 
  for at, col1 in pairs(cols1.all) do
      if not col1.txt:find "X$" then
          (col1.txt:find "[-!+]$" and cols1.y or cols1.x)[at]=col1 end end
  return cols1 end

function l.cols(cols1, t)
  for _, col1 in pairs(cols1.all) do
    l.col(col1, t[col1.at]) end end

-- ## Row ---------------------------------------------
function l.ROW(t) return {cells=t} end

-- ## Cols and Rows ---------------------------------------------
function l.DATA(src,    data1)
  data1={rows={},cols=nil}
  if   type(src)=="string"
	then for _,t    in lib.csv(src) do l.data(data1,l.ROW(t)) end
	else for _,row1 in pairs(src)   do l.data(data1, row1) end end
	return data1 end

function l.data(data1, row1)
  if   data1.cols
  then l.cols(data1.cols, row1.cells)
       data1.rows[ 1+#data1.rows ] = row1
  else data1.cols = l.COLS(row1.cells) end end

function l.stats(data1, what, decs, cols1, t)
    t = { N = #data1.rows }
    for _, col1 in pairs(cols1 or data1.cols.y) do
        t[col1.txt] = lib.rnd((what or l.mid)(col1), decs) end
    return t end

function l.clone(data1,  rows,     data2)
  data2 = DATA({ROW(data1.col.names)})
  for _,row in pairs(rows or {}) do  data(data2,row)) end 
  return data2 end

return l
