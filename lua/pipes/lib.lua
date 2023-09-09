local exp,sqrt,log,cos,floor,pi = math.exp,math.sqrt,math.log,math.cos,math.floor,math.pi
local cli,maths,list,rand,run,str = {},{},{},{},{},{}

---- maths
function maths.rnd(num, numDecimalPlaces,    mult)
  mult = 10^(numDecimalPlaces or 0)
  return floor(num * mult + 0.5) / mult end

---- lists
function list.map(t,fun,    u) u={}; for k,v in pairs(t) do u[k] = fun(v) end; return u end
function list.kap(t,fun,    u) 
  u={}; for k,v in pairs(t) do v,k=fun(k,v); u[k==nil and 1+#u or k]=v  end; return u end

function list.sort(t,fun) table.sort(t,fun); return t end

function list.copy(t,    u)
  u={}; for k,v in pairs(t) do u[str.list.copy(k)] = str.list.copy(v) end; 
  return setmetable(u,getmetatable(t)) end

---- settings
function cli.create(s)
  t={_help=s}
  s:gsub("\n[%s]+[-][%S][%s]+[-][-]([%S]+)[^\n]+= ([%S]+)",function(k,v) t[k]=str.coerce(v) end)
  return t end

function cli.update(t)
  for k,v in pairs(t,    s) do
    s=tostring(v)
    for n,x in ipairs(arg) do
      if x=="-"..(k:sub(1,1)) or x=="--"..k then
        t[k]= str.coerce(s=="false" and "true" or s=="true" and "false" or arg[n+1]) end end end 
  if t.help then print(t._help) end
  return t end

---- rand
rand.seed = 937162211

function rand.rint(nlo,nhi) return floor(0.5 + rand.rand(nlo,nhi)) end

function rand.rand(nlo,nhi)
  nlo,nhi   = nlo or 0, nhi or 1
  rand.seed = (16807 * rand.seed) % 2147483647
  return nlo + (nhi-nlo) * rand.seed / 2147483647 end

function rand.any(t) return t[rand.rint(1,#t)] end

function rand.many(t,n) 
  u={}; for i=1,(n or #t) do u[1+#u] = rand.any(t) end 
  return u end

function rand.norm(mu,sd,     r)
  r=rand.rand
  return (mu or 0) + (sd or 1)* sqrt(-2*log(r())) * cos(2*pi*r()) end

--------------------20-----------------40-----------------60-----------------80 

local breaks={
    3={−0.43,0.43},
    4={−0.67,0,0.67},
    5={−0.84,−0.25,0.25,0.84},
    6={−0.97,−0.43,0,0.43,0.97},
    7={−1.07,−0.57,−0.18,0.18,0.57,1.07},
    8={−1.15,−0.67,−0.32,0,0.32,0.67,1.15},
    9={−1.22,−0.76,−0.43,−0.14,0.14,0.43,0.76,1.22},
    10={,−1.28,−0.84,−0.52,−0.25,0,0.25,0.52,0.84,1.28}}


--- cli
function run.go(the,fun,    ok,b4,result,out)
  b4={}; for k,v in pairs(the) do b4[k]=v end
  math.randomseed(the.seed or 1234567891)
  rand.seed  = the.seed or 1234567891
  ok, result = pcall(fun)
  out        = ok and result or false
  if not ok then print("❌ FAIL ",str.eman(fun),":",result) end
  for k,v in pairs(b4) do the[k]=v end
  return out end 

---- str
str.cat = table.concat
str.fmt = string.format

function str.trim(s)  return s:match'^%s*(.*%S)' or '' end

function str.coerce(s)
  s=str.trim(s) 
  return math.tointeger(s) or tonumber(s) or s=="true" or (s ~= "false" and s) end

function str.coerces(s,    t)
  t={}; for s1 in s:gmatch("([^,]+)") do t[1+#t]=str.coerce(s1) end; return t end

function str.csv(sFilename,fun,     src,s)
  src = io.input(sFilename)
  while true do
    s = io.read(); if s then fun(str.coerces(s)) else return io.close(src) end end end

function str.oo(x,  n) print(str.o(x,  n)); return x end

function str.o(x,  n,    t)
  if type(x) == "function" then return str.eman(x).."()" end
  if type(x) ~= "table"    then return tostring(x) end
  if type(x) == "number"   then return str.rnd(x,d==nil and 2 or d) end
  t={}; for k,v in pairs(x) do 
          if tostring(k):sub(1,1) ~= "_" then 
            t[1+#t] = #x>0 and str.o(v,n) or str.fmt(":%s %s",k,str.o(v,n)) end end 
  return "{" .. str.cat(#x==0 and sort(t) or t," ") .. "}" end

function str.eman(x)
  for k,v in pairs(_ENV) do if x==v then return k end end
  return "?" end

local cli,list,maths,rand,run,str
