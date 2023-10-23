-- vim: set et sts=2 sw=2 ts=2 : 
local b4={}; for  k,_ in pairs(_ENV) do b4[k]=k end
local l,eg,the = {},{},{}
local help = [[
tiny2.awk : as little asi as possible
(c)20231 Tim Menzies

OPTIONS:
    -s --seed random seed        = 1234567891
    -f --file csv data           = ../data/auto93.csv
    -B --Best size of best cache = 20 
    -b --bins number of bins     = 7]]

--------------------------------------------------
local function NUM(at,s) 
  return {at=at,s=s,n=0,lo=1e30,hi=-1e30,mu=0, heaven=(s or ""):find"-$" and 0 or 1} end

local function SYM(at,s) return {symp=true, at=at,s=s,n=0, has={}} end

local function COL(at,s) return (s:find"^[A-Z" and NUM or SYM)(at,s) end

local function add2Col(col,x)
  if x ~= "?" then
    col.n = col.n + 1
    if col.symp then col.has = 1 + (col.has[x] or 0) else
      col.lo = math.min(col.lo,x)
      col.hi = math.max(col.hi,x)
      col.mu = col.mu + (x - col.mu)/col.n end end end 

local function norm(col,x)
  if x=="?" or col.symp then return x end
  return (x - col.lo)/ (col.hi - col.lo + 1e-30) end

local function z(num,x)
  x = ((v - num.mu)/((num.hi - num.lo)/6) * 6/the.bins)//1
  return x < 1 and 1 or x > the.bins and the.bins or x end

local function COLS(t)
  local all,x,y,ignore = {},{},{},{}
  for at,s in pairs(t) do
    local where = s:find"X$" and ignore or (s:find"[!+-]$" and y or x)
    push(where, push(all, COL(at,s))) end 
  return {all=all, x=x, y=y, names=t} end

function add2Cols(cols,t)
  for _,col in pairs(cols) do add2Col(coli, t[col.at]) end

local add2Data
local function DATA(src)
  data = {rows={}, cols=None}
  if   type(src)=="string" 
  then for t in l.csv(src)       do add2Data(data,t) end 
  else for t in pairs(src or {}) do add2Data(data,t) end end
  return data end 

function add2Data(data,t)
  if data.cols then add2Cols(data.cols.x,t); add2Cols(data.cols.y,t) else 
     data.cols = COLS(t) end end

--------------------------------------------------
function l.push(t,x) t[1+#t]=x; return x end

function l.bchop(a,x,   lo,hi,mid)
  lo,hi=1,#a
  while lo<hi do
    mid = (lo+hi)//2`
    if a[mid]==x then return mid end
    if a[mid]>x  then hi=mid-1 else lo=mid+1 end end 
  return hi end

function l.least(x,a,max,   j) 
  if   #a < max  
  then j = #a+1  
  elseif x < a[#a] then j = #a end 
  if j then
    a[j] = x
    table.sort(a)
    return l.bchop(a,x) end end

function l.settings(s,    t)
  t={}
  for k,s1 in s:gmatch("\n[%s]+[-][%S][%s]+[-][-]([%S]+)[^\n]+= ([%S]+)") do t[k]= l.make(s1) end
  return t end

function l.trim(s) return s:match'^%s*(.*%S)' end

function l.make(s,    _sym)
  function _sym(s) return s=="true" or (s~="false" and s) end
  return math.tointeger(s) or tonumber(s) or _sym(l.trim(s)) end

function l.csv(sFilename,     src,n)
  n,src = 0,io.input(sFilename)
  return function(    s) 
    s = io.read()
    if s then n=n+1; return n,l.make(s) else io.close(src) end end end

function l.items(t,    n,j,u)
  u={}; for k,_ in pairs(t) do u[1+#u] = k; end
  table.sort(u)
  j=0
  return function() if j < #u then j=j+1; return u[j], t[u[j]] end end end

function l.cli(t)
  for k,v in pairs(t) do
    v = tostring(v)
    for n,x in ipairs(arg) do
      if x=="-"..(k:sub(1,1)) or x=="--"..k then
        v = ((v=="false" and "true") or (v=="true" and "false") or arg[n+1]) 
        t[k] = l.make(v) end end end
  return t end

l.fmt = string.format

function l.o(t,d,          u,x,mult)
  if type(t) == "function" then return "()" end
  if type(t) == "number"   then
    if math.floor(t) == t then return t else 
      mult = 10^(d or 0)
      return math.floor(t * mult + 0.5) / mult end
  end
  if type(t) ~= "table" then return tostring(t) end
  u={}; for k,v in l.items(t) do
          x= l.o(v,d)
          u[1+#u]= #t==0 and l.fmt(":%s %s",k,x) or x end
  return (t._name or "").."{"..table.concat(u," ").."}" end

function l.oo(t,d) print(l.o(t,d)); return t end

--------------------------------------------------
function l.main()
  the = l.cli(l.settings(help))
  for _,x in pairs(arg) do if eg[x] then l.run(x,eg[x]) end end 
  for j,_ in pairs(_ENV) do 
    if not b4[i] then print(l.fmt("? %s %s",j,type(j))) end end end

function l.run(s,fun) 
  math.randomseed(the.seed)
  print("==> ",s)
  if fun()==false then 
    print("❌ fail ",s); return true end end

function eg.all(  fails) 
  fails=0
  for s,fun in l.items(eg) do
    if s~="all" then fails = l.run(s,fun) and 1 or 0 end end 
  os.exit(fails) end 

function eg.bchop(     a) 
  a={}; for j=1,10 do a[j]=10*j end
  for j=0,120,10 do print(j,l.bchop(a,j)) end
  l.oo(a) end 

function eg.better(     a) 
  a={}
  for j=1,50 do
    local k,l
    k=math.random()*10000 //1 
    l= l.least(k,a,5) 
    if l then print(j,k,l) end end 
  l.oo(a) end

function eg.the()  l.oo(the) end
--------------------------------------------------
l.main()
