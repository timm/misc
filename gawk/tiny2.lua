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
local function DATA() return {num={n={},lo={},hi={},mu={},
                        sym={has={}}, name={}
                        x={}, y={}, rows={}}

local function head(i,t)
  i.name=t
  n=i.num
  for c,s in pairs(t) do
    if     s:find"-$" then n.y[c] = 0 
    elseif s:find"+$" then n.y[c] = 1 
    else   n.x[c]=c end
    if s:find"^[A-Z]" then n.n[c]=0; n.lo[c]=1e30; n.hi[c]= -1e30 end end end
--------------------------------------------------
function l.bchop(a,x,   lo,hi,mid)
  lo,hi=1,#a
  while lo<hi do
    mid = (lo+hi)//2
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
