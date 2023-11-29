local lib=require"lib"
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
    if   col1.isSym 
    then t[x] = 1+(t[x] or 0) 
    else t[1+#t]=x; col1.unsorted=true end end end

function l.has(col1)
  if col1.unsorted then table.sort(col1.has); col1.unsorted=false end
  return col1.has end

function l.mid(col1) 
  return (col1.isSym and lib.mode or lib.median)(l.has(col1)) end

function l.div(col1) 
  return (col1.isSym and lib.entropy or lib.stdev)(l.has(col1)) end

-- ## Many Cols ---------------------------------------------
function l.COLS(t,     cols1,t)
  cols1= {x={}, y={}, names=t, all=lib.kap(t, l.COL)} 
  for col1 in pairs(cols1.all) do
    if not col1.txt:find"X$" then
      t = col1.txt:find"[+-!]" and cols1.y or cols1.x
      t[1+#t] = col1 end  end
  return cols1 end

function l.cols(cols1,row1)
  for _,col1 in cols1.all do l.col(col1, row1[col1.at]) end end

-- ## Cols and Rows ---------------------------------------------
function l.DATA(src,    data1) 
  data1={rows={},cols=nil}
  if type(src)=="string"
	then for t     in lib.csv(src) do l.data(data1,l.ROW(t)) end
	else for _,row1 in pairs(src) do l.data(data1, row1) end end
	return data1 end

function l.data(data1, row1)
  if   data1.cols
  then l.cols(data1.cols, row1)
       data1.rows[ 1+#data1.rows ] = row1
  else data1.cols = l.COLS(row1.cells) end end

function l.stats(data1,  what,decs,cols1,      t)
  t={N=#data1.rows}
  for _,col1 in pairs(cols1) do 
    t[col1.txt] = lib.rnd((what or l.mid)(col1),decs) end 
  return t end

return l
