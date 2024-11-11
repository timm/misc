BIG = 1E32
local coerce,kap,lt,map,push,o,sort
local abs, cos,log,max = math.abs, math.cos, math.log, math.max
local min,pi,R,sqrt = math.min, math.pi, math.random, math.sqrt

local the = {rseed=1234567891,train="../../data/auto93.csv"}

---------------------------------------------------------------------
local Num,Sym,Data,Cols

function Num(name,at) 
  return {is="Num", name=name, at=at, n=0, sd=0, mu=0, m2=0, 
          lo=BIG, hi=-BIG, goal = (name or ""):find"-$" and 0 or 1} end

function Sym(name,at) 
  return {is="Sym",name=name,at=at,n=0,has={}, mode=0, most=0} end

function Data(names)
  return {is="Data",rows=rows, col=Cols(names)} end

function Cols(names,    i)
  i = {is="Cols", names=names, all={}, x={}, y={}}
  for at,name in pairs(i.names) do
    col = push(i.all, name:find"^[A-Z]" and Num or Sym)(name,at)
    if not name:find"X$" then
      push(name:find"[!+-]$" and i.y or i.x,col) end end
  return i end

---------------------------------------------------------------------
local add,addData,csv2Data,cloneData

function addCol(i,x,     d)
  if x=="?" then return end
  i.n = i.n + 1
  if i.is=="Sym" then
    i.has[x] = 1 + (i.has[x] or 0)
    if i.has[x] > i.most then i.most, i.mode=i.has[x], x end 
  else
    d    = x - i.mu
    i.mu = i.mu + d/i.n
    i.m2 = i.m2 + d*(x - i.mu) 
    i.sd = i.n < 2 and 0 or (i.m2 / (i.n - 1))^0.5
    i.hi = max(x, i.hi)
    i.lo = min(x, i.lo) end  end

function addData(i,row)
  for k,v in pairs(row) do addCol(i.cols.all[k], v) end
  push(i.rows, row) end

function csv2Data(file,   t,i)
  for line in io.lines(file) do  
    print(line)
    t={}
    for s in line:gmatch"([^,]+)" do t[1+#t]=coerce(s) end
    if i then addData(i,t) else i=Data(t) end end 
  return i end

function cloneData(i, j,rows)
  j = Data(i.cols.names)
  for _,row in pairs(rows or {}) do addData(j, row) end
  return j end

---------------------------------------------------------------------
function normal(mu,sd) 
  return (mu or 0) + (sd or 1) * sqrt(-2*log(R())) * cos(2*pi*R()) end

function push(t,x)  t[1+#t]=x; return x end

function kap(t,f,   u) u={}; for k,v in pairs(t) do u[1+#u]=f(k,v) end; return u end
function map(t,f,   u) u={}; for _,v in pairs(t) do u[1+#u]=f(  v) end; return u end

function sort(t,fn) table.sort(t,fn); return t end
function lt(x)      return function(a,b) return a[x] < b[x] end end

function o(x,     f,g) 
  f = function(x) return #x>0 and map(x,o) or sort(kap(x,g)) end
  g = function(k,v) if k ~= "is" then return string.format(":%s %s",k,o(x[k])) end end
  return type(x)=="number" and string.format("%g",x) or  
         type(x)~="table"  and tostring(x) or 
         (x.is or "") .. "(" .. table.concat(f(x)," ") .. ")" end 

function coerce(s,     other,trim) 
  trim  = function(s) return s:match"^%s*(.-)%s*$" end
  other = function(s) return s=="true" and true or s ~= "false" and s end
  return math.tointeger(s) or tonumber(s) or other(trim(s)) end

---------------------------------------------------------------------
local ok={}

function ok.o(_) print(o(the)) end

function ok.num(_, n) 
  n = Num(); for _=1,1000 do addCol(n,normal(20,2)) end; print(o(n)) end

function ok.sym(_, s) 
  s = Sym(); for _,x in pairs{"a","a","a","a","b","b","c"} do addCol(s,x) end; print(o(s)) end

function ok.data(_, d)
  d = csv2Data(the.train) end
---------------------------------------------------------------------
if arg[0] =="b1.lua" then 
  math.randomseed(the.rseed)
  local fails = 0
  for k,s in pairs(arg) do
    s = s:sub(3)
    if ok[s] and false==ok[s](arg[k+1]) then fails=fails+1 end end
  os.exit(fails) end 


