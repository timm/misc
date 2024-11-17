local trim,coerce
local log,exp,pi,max,min = math.log, math.exp, math.pi, math.max, math.min

local the = {
  guess ={ acquire= "exploit",
           enough = 50000000,
           start  = 4,
           stop   = 30},
  bayes  = {k     = 1,
            m     = 2},
  stats = {bootstraps = 512,
            delta     = 0.197,
            conf      = 0.05,
            cohen     = 0.35},
  big   = 1E32,
  p     = 2,
  rseed = 1234567891, 
  train = "../../moot/optimize/misc/auto93.csv",
  Test  = 0.33}

-------------------------------------------------------------------------------
function sort(t,f) table.sort(t,f); return t end
function trim(s)   return s:match"^%s*(.-)%s*$" end
function coerce(s) return math.tointeger(s) or tonumber(s) or trim(s) end
function map(t,f)  u={}; for _,v in pairs(t) do u[1+#u]=f(v) end; return u end

function csv(file,     src) --> str --> func
  if file and file ~="-" then src=io.input(file) end
  return function(     s,t)
    s = io.read()
    if s then
      t={}; for s1 in s:gmatch"([^,]+)" do t[1+#t]=coerce(s1) end; return t 
    else 
      if src then io.close(src) end end end end

function o(x)
  if type(x)=="number" then return string.format("%.2g",x) end
  if type(x)~="table"  then return tostring(x) end
  x = #x==0 and okeys(x) or map(x,o)
  return (x.is or "") .. "(" .. table.concat(x," ") .. ")"  end
 
function okeys(t,     u)
  u={}; for k,v in pairs(t) do 
          if k ~= "is" then u[1+#u]=table.concat(":%s %s",k,o(v)) end end
  return sort(u) end

-------------------------------------------------------------------------------
local Num,Sym,Data,Cols

function Num(s,at) 
  return {is=Num, name=s, at=at, n=0, sd=0, mu=0, m2=0, lo=the.BIG, hi=-the.BIG, 
          goal = (s or ""):find"-$" and 0 or 1} end

function Sym(s,at) 
  return {is=Sym, name=s, at=at, n=0, has={}, mode=0, most=0} end

function Data(names) 
  return {is=Data,rows={}, cols=Cols(names)} end

function Cols(names,      i,col)
  i = {is=Cols, names=names, all={}, x={}, y={}, klass=nil} 
  for j,s in pairs(names) do
    col = push(i.all, (s:find"^[A-Z]" and Num or Sym)(s,j))
    if not s:find"X$" then
      if s:find"!$" then i.klass = it end
      push(s:find"[!+-]$" and i.y or i.x, col) end end
  return i end

function add(i,x,     d) 
  if type(x)=="table" then 
     for _,y in pairs(x) do add(i,y) end; return end
  if x == "?" then return end
  i.n = i.n + 1
  if i.is==Sym then
    i.has[x] = 1 + (i.has[x] or 0)
    if i.has[x] > i.most then i.most, i.mode=i.has[x], x end
    return end 
  d    = x - i.mu
  i.mu = i.mu + d/i.n
  i.m2 = i.m2 + d*(x - i.mu) 
  i.sd = i.n < 2 and 0 or (i.m2 / (i.n - 1))^0.5
  i.hi = max(x, i.hi)
  i.lo = min(x, i.lo) end 

function adds(i,row)
  for k,v in pairs(row) do add(i.cols.all[k], v) end
  return i end

function reads(i,file) 
  for row in csv(file) do add(i,row) end 
  return i end

