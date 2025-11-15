local the = {seed=1234567891, goal=1, leaf=2,
             file="../../moot/optimize/misc/auto93.csv"}

local csv, pat, push,sort -- defined later
local is_num,is_y,add,add1, Sym,Num,Cols,Data ----------------------------------
function is_num(s) return s:find"^[A-Z]" end
function is_y(s)   return s:find"[+-!]$" end

function Sym(i,s) return {at=i or 0, txt=s or "", n=0, has={}} end
function Num(i,s) return {at=i or 0, txt=s or "", n=0, has={}, nump=true,
                          lo=1e32, hi=-1e32,
                          goal=(s or ""):find"-$" and 0 or 1} end

function Cols(names,    cols,col)
  cols = {names=names, all={}, x={}, y={}}
  for i,s in pairs(names) do 
    col = (is_num(s) and Num or Sym)(i,s)
    push(cols.all, col)
    push(is_y(s) and cols.y or cols.x, col) end
  return cols end 

function Data(src)
  return adds({rows={},cols=nil}, src) end

function adds(data, src)
  if   type(src)=="string" 
  then for   row in csv(src)   do add(data,row) end 
  else for _,row in pairs(src) do add(data,row) end 
  end 
  for _,col in pairs(data.cols.all) do
    if col.nump then 
      col.has = sort(col.has) 
      col.lo, col.hi = col.has[1], col.has[#col.has] end end
  return data end
 
function add(data,row)
  if   data.cols 
  then for _,col in pairs(data.cols.all) do add1(col, row[col.at]) end
  else data.cols = Cols(row) end end

function add1(col,v)
  if v ~= "?" then
    col.n = col.n + 1
    col.has[1+#col.has] = v end end

function clone(data, rows)
  return adds(Data{data.cols.names}, rows or {}) end

local fmt,lat,dat,cat -----------------------------------------
fmt=string.format

function pat(t) print(cat(t)); return t end

function cat(t)
  if type(t)~="table"  then return tostring(t) end
  if type(t)=="number" then return fmt(t % 1==0 and "%d" or "%.3f",t) end
  return "{"..table.concat(#t>0 and lat(t,{}) or sort(dat(t,{}))," ").."}" end

function lat(t, u) 
  for i,v in pairs(t) do u[i]=cat(v) end; return u end 

function dat(t, u) 
  for k,v in pairs(t) do u[1+#u] = fmt(":%s %s",k,cat(v)) end; return u end 

local known,at,map --------------------------------------------------------
function push(t,x) t[1+#t]=x; return x end
function sort(t,fn) table.sort(t,fn); return t end
function known() return function(x) if x ~= "?" then return x end end end
function at(n) return function(t) return t[n] end end

function map(t,fn,  u) 
  u={};for i,v in pairs(t) do u[1+#u]=fn(v)end;return u end 

local is,cells ----------------------------------------------------------------
function is(s) return tonumber(s) or s end
function cells(s,  t)
  t={}; for s1 in s:gmatch"([^,]+)" do t[1+#t]=is(s1) end; return t end

function csv(file)
  local fp = file and io.open(file) or io.input()
  return function() local s=fp:read(); return s and cells(s) end end

-- function nums(rows,n) return sort(map(rows,known(at(n)))) end

local eg = {} -----------------------------------------------------------------
eg.seed= function(x) 
  the.seed= is(x); math.randomseed(the.seed); print(math.random()) end

eg.goal= function(x) the.goal= is(x) end
eg.leaf= function(x) the.leaf= is(x) end
eg.rows= function(_) pat(Data(the.file).cols.x[2]) end

local function main()
  for i,s in pairs(arg or {}) do
    local key = s:gsub("^%-+", "")  
    if eg[key] then eg[key](arg[i+1]) end end end

math.randomseed(the.seed)
if arg[0]:find"irq.lua" then main() end

return {Data=Data, Num=Num, Sym=Sym, csv=csv, pat=pat, cat=cat}
