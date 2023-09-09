----------1---------2---------3---------4----------5----------6----------7---------8---------9
---- lint
local lint={}
lint.b4={}; for k,v in pairs(_ENV) do b4[k]=k end

function lint.rogues() 
  for k,v in pairs(_ENV) do 
    if not b4[k] then print("E> rogue",k," of " type(v)) end end end

---- names
local exp,sqrt,log,cos,floor,pi = math.exp,math.sqrt,math.log,math.cos,math.floor,math.pi
local lint,list,maths,rand,run,settings,str = {},{},{},{},{},{},{}
local obj

---- object
local id=0
local function obj(s,    t) 
  t = {_name=s} 
  t.__index = t 
  return setmetatable(t, {
     __call=function(_,...)
        id = id + 1
        local i = setmetatable({_id=id},t);
        return setmetatable(t.init(i,...) or i,t) end}) end

---- maths
function maths.rnd(num, numDecimalPlaces,    mult)
  mult = 10^(numDecimalPlaces or 0)
  return floor(num * mult + 0.5) / mult end

---- lists
function list.map(t,fun,    u) 
  u={}; for k,v in pairs(t) do u[k] = fun(v) end; return u end

function list.kap(t,fun,    u) 
  u={}; for k,v in pairs(t) do v,k=fun(k,v); u[k==nil and 1+#u or k]=v  end
  return u end

function list.sort(t,fun) 
  table.sort(t,fun); return t end

function list.copy(t,    u)
  u={}; for k,v in pairs(t) do u[str.list.copy(k)] = str.list.copy(v) end; 
  return setmetable(u,getmetatable(t)) end

---- settings
function settings.create(s)
  t={_help=s}
  s:gsub("\n[%s]+[-][%S][%s]+[-][-]([%S]+)[^\n]+= ([%S]+)",
         function(k,v) t[k]=str.coerce(v) end)
  return t end

function settings.update(t)
  for k,v in pairs(t,    s) do
    s=tostring(v)
    for n,x in ipairs(arg) do
      if x=="-"..(k:sub(1,1)) or x=="--"..k then
        t[k]= str.coerce(s=="false" and "true" or 
                         s=="true"  and "false" or arg[n+1]) end end end 
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

--- cli
function run.run(settings,name, fun,    ok,b4,result,out)
  b4={}; for k,v in pairs(settings) do b4[k]=v end
  math.randomseed(settings.seed or 1234567891)
  rand.seed  = settings.seed or 1234567891
  ok, result = pcall(fun)
  out        = ok and result or false
  if not ok then print("❌ FAIL ",name,":",result) end
  for k,v in pairs(b4) do settings[k]=v end
  return out end 

function run.runs(settings)
  for k,fun in pairs(_ENV) do
    pre,name = name:match"(%w+)_(.+)"
    if pre=="eg_" and "settings.go"==name or "settings.go"=="all" then
       run.run(settings,fun
    if k:find"^eg_" then
---- str
str.cat = table.concat
str.fmt = string.format

function str.trim(s)  return s:match'^%s*(.*%S)' or '' end

function str.coerce(s)
  s=str.trim(s) 
  return (math.tointeger(s) or tonumber(s) or 
          s=="true" or (s ~= "false" and s)) end

function str.coerces(s,    t)
  t={}; for s1 in s:gmatch("([^,]+)") do t[1+#t]=str.coerce(s1) end
  return t end

function str.eman(x)
  for k,v in pairs(_ENV) do if x==v then return k end end
  return "?" end

function str.csv(sFilename,fun,     src,s)
  src = io.input(sFilename)
  while true do
    s = io.read(); if s then fun(str.coerces(s)) else return io.close(src) end end end

function str.oo(x,  n) 
  print(str.o(x,  n)); return x end

function str.o(x,  n,    t)
  if type(x) == "function" then return str.eman(x).."()" end
  if type(x) ~= "table"    then return tostring(x) end
  if type(x) == "number" then
    if math.floor(x) ~= x then 
      x = str.rnd(x, d or the.decimals or  2) end
    return tostring(x) end 
  t={}; for k,v in pairs(x) do 
          if tostring(k):sub(1,1) ~= "_" then 
            if #x>0 then t[1+#t] = str.o(v,n) 
            else         t[1+#t] = str.fmt(":%s %s", k, str.o(v,n)) end end 
  return (t._name or "").. "{" .. str.cat(#x==0 and sort(t) or t," ") .. "}" end

return lint,list,maths,obj,rand,run,settings,str 
