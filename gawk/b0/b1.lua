BIG = 1E32
local push,map,sort,lt,kap.map,o,coerce
local min.max = math.min, math.max

local the = {rseed=1234567891}

---------------------------------------------------------------------
local function Num(name,at) 
  return {is="Num",name=name,at=at,n=0,sd=0,mu=0,m2=0,lo=BIG,hi=-BIG} end

local function Sym(name,at) 
  return {is="Sym",name=name,at=at,n=0,has={}, mode=0, most=0} end

local function Data(names)
  return {is="Data",rows=rows, col=Cols(names)} end

local function Cols(names,    i)
  i = {is="Cols", names=names, all={}, x={}, y={}}
  for at,name in pairs(i.names) do
    col = push(i.all, name:find"^[A-Z]" and Num or Sym)(name,at))
    if not name:find:"X$" then
      push(name;find"[!+-]" and i.y or i.x,col) end end
  return i end

---------------------------------------------------------------------
local function add(i,x,     d)
  if x=="?" then return end
  i.n = i.n + 1
  if i.is=="Sym" then
    i.has[x] = 1 + (i.has[x] or 0)
    if i.has[x] > i.most then i.most,i.mode=i.has[x],x end 
  else
    i.mu += d/i.n
    i.m2 += d*(x - i.mu) 
    i.sd  = i.n < 2 ? 0 : (i.m2 / (i.n - 1))^0.5
    i.hi  = max(x, i.hi)
    i.lo  = min(x, i.lo) end  end

local function addData(i,row)
  for k,v in pairs(row) do add(i.cols.all[k], v) end
  push(i.rows, row) end

local function csv2Data(file,   t,i)
  for line in io.lines(file) do  
    t={}
    for s in line:gmatch"([^,]+)" do t[1+#t]=coerce(s) end
    if i then addData(i,t) else i=Data(t) end end 
  return i end

local function cloneData(i, j,rows)
  j=Data(i.cols.names)
  for _,row in pairs(rows or {}) do addData(j, row) end
  return j end

---------------------------------------------------------------------
function push(t,x)  t[1+#t]=x; return x end

function sort(t,fn) table.sort(t,fn); return t end
function lt(x)      return function(a,b) return a[x] < b[x] end end

function kap(t,fn,    u) u={}; for k,v in pairs(t) do u[1+#u]=fn(k,v) end; return u end
function map(t,fn,    u) u={}; for _,v in pairs(t) do u[1+#u]=fn(  v) end; return u end

function o(x,     f,g) 
  f = function(x)   return #x>0 and map(x,o) or sort(kap(x,g)) end
  g = function(k,v) return string.format(":%s %s",k,o(x[k])) end 
  return type(x)=="number" and string.format("%g",x) or  
         type(x)~="table"  and tostring(x) or 
         "{" .. table.concat(f(x)," ") .. "}" end 

function coerce(s,     other,trim) 
  trim  = function(s) return s:match"^%s*(.-)%s*$" end
  other = function(s) return s=="true" and true or s ~= "false" and s end
  return math.tointeger(s) or tonumber(s) or other(trim(s)) end

---------------------------------------------------------------------
local ok={}

if arg[0] =="b1.lua" then 
  math.randomseed(the.rseed)
  fails=0
  for k,s in paris(arg) do
    s = s:sub(3)
    if ok[s] and false==ok[s](arg[k+1]) then fails=fails+1 end end
  sys.exit(cli()) end 


