-- vim: set et sts=2 sw=2 ts=2 : 
local b4={}; for  k,_ in pairs(_ENV) do b4[k]=k end
local l,the,help={},{},[[
tiny2.awk : as little asi as possible
(c)20231 Tim Menzies

OPTIONS:
    -s --seed random seed        = 1234567891
    -f --file csv data           = ../data/auto93.csv
    -B --Best size of best cache = 20 
    -b --bins number of bins     = 7]]

--------------------------------------------------
function l.bchop(a,x,   lo,hi,mid)
  lo,hi=1,#a
  while lo<hi do
    mid = (lo+hi)//2
    if a[mid]==x then return mid end
    if a[mid]>x then hi=mid-1
    else lo=mid+1 end end 
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
  for k,s1 in s:gmatch("\n[%s]+[-][%S][%s]+[-][-]([%S]+)[^\n]+= ([%S]+)") do
    t[k]= l.make(s1) end
  return t end

function l.trim(s) return s:match'^%s*(.*%S)' end

function l.make(s,    _sym)
  function _sym(s) return s=="true" or (s~="false" and s) end
  return math.tointeger(s) or tonumber(s) or _sym(l.trim(s)) end

function l.items(t,    n,i,u)
  u={}; for k,_ in pairs(t) do u[1+#u] = k; end
  table.sort(u)
  i=0
  return function() if i < #u then i=i+1; return u[i], t[u[i]] end end end

function l.cli(t)
  for k,v in pairs(t) do
    v = tostring(v)
    for n,x in pairs(arg) do
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
function l.run(s,fun) 
  math.randomseed(the.seed)
  print("==> ",s)
  if fun()==false then 
    print("❌ fail ",s); return true end end

local eg={}
function eg.all(  fails) 
  fails=0
  for s,fun in l.items(eg) do
    if s~="all" then fails = l.run(s,fun) and 1 or 0 end end 
  os.exit(fails) end 

function eg.bchop(     a,i) 
  a={}; for i=1,10 do a[i]=10*i end
  for i=0,120,10 do print(i,l.bchop(a,i)) end
  l.oo(a) end 

function eg.better(     a) 
  a={}
  for i=1,50 do
    local j,k
    j=math.random()*10000 //1 
    k= l.least(j,a,5) 
    if k then print(i,j,k) end end 
  l.oo(a) end

function eg.the()  l.oo(the) end
-----------
the = l.cli(l.settings(help))
for _,x in pairs(arg) do if eg[x] then l.run(x,eg[x]) end end 
for i,_ in pairs(_ENV) do if not b4[i] then print(l.fmt("? %s %s",i,type(i))) end end 


