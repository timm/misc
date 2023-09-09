--       __                    ___                        
--      /\ \__                /\_ \                       
--    __\ \ ,_\   ___         \//\ \    __  __     __     
--  /'__`\ \ \/  /'___\         \ \ \  /\ \/\ \  /'__`\   
-- /\  __/\ \ \_/\ \__/       __ \_\ \_\ \ \_\ \/\ \L\.\_ 
-- \ \____\\ \__\ \____\     /\_\/\____\\ \____/\ \__/.\_\
--  \/____/ \/__/\/____/     \/_/\/____/ \/___/  \/__/\/_/
--                                                        
-------------------- ------------------- --------------------- -------------------- ----------
-- | o ._  _|_ 
-- | | | |  |_ 
            
local lint={}
lint.b4={}; for k,v in pairs(_ENV) do lint.b4[k]=k end

function lint.rogues() 
  for k,v in pairs(_ENV) do 
    if not lint.b4[k] then print("E> rogue",k," of ",type(v)) end end end
-------------------- ------------------- --------------------- -------------------- ----------
-- ._   _. ._ _   _   _ 
-- | | (_| | | | (/_ _> 

local exp,sqrt,log,cos,floor,pi = math.exp,math.sqrt,math.log,math.cos,math.floor,math.pi
local lint,list,maths,rand,settings,str,test = {},{},{},{},{},{},{}
local obj
-------------------- ------------------- --------------------- -------------------- ----------
--  _  |_   o  _   _ _|_  _ 
-- (_) |_)  | (/_ (_  |_ _> 
--         _|               

local id=0
local function obj(s,    t) 
  t = {_name=s} 
  t.__index = t 
  return setmetatable(t, {
     __call=function(_,...)
        id = id + 1
        local i = setmetatable({_id=id},t);
        return setmetatable(t.init(i,...) or i,t) end}) end
-------------------- ------------------- --------------------- -------------------- ----------
-- ._ _   _. _|_ |_   _ 
-- | | | (_|  |_ | | _> 

function maths.rnd(num, numDecimalPlaces,    mult)
  if math.floor(num) == num then return num end
  mult = 10^(numDecimalPlaces or 0)
  return floor(num * mult + 0.5) / mult end
-------------------- ------------------- --------------------- -------------------- ----------
-- | o  _ _|_ 
-- | | _>  |_ 

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
-------------------- ------------------- --------------------- -------------------- ----------
--  _  _  _|_ _|_ o ._   _   _ 
-- _> (/_  |_  |_ | | | (_| _> 
--                       _|    

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
-------------------- ------------------- --------------------- -------------------- ----------
-- ._  _. ._   _| 
-- |  (_| | | (_| 

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
-------------------- ------------------- --------------------- -------------------- ----------
-- _|_  _   _ _|_ 
--  |_ (/_ _>  |_ 

function test.Maybe(settings,name, fun,    ok,b4,result,out)
  b4={}; for k,v in pairs(settings) do b4[k]=v end
  math.randomseed(settings.seed or 1234567891)
  rand.seed  = settings.seed or 1234567891
  ok, result = pcall(fun)
  out        = ok and result or false
  if not ok then print("❌ FAIL ",name,":",result) end
  for k,v in pairs(b4) do settings[k]=v end
  return result==false and 1 or 0 end 

function test.Run(settings,     tag,fails)
  fails=0
  for name,fun in pairs(test) do
    if name:find"^[a-z]" then
      tag = name:match"(%w+).*"
      if settings.go==tag or settings.go=="all" then
        fails = fails + test.Maybe(settings,tag,fun)  end end end
  os.exit(fails) end
-------------------- ------------------- --------------------- -------------------- ----------
--  _ _|_ ._ 
-- _>  |_ |  
           
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

function str.csv(sFilename,fun,     src,s)
  src = io.input(sFilename)
  while true do
    s = io.read(); if s then fun(str.coerces(s)) else return io.close(src) end end end

function str.eman(x)
  for k,v in pairs(_ENV) do if x==v then return k end end
  return "?" end

function str.o(x,  n,    t)
  if type(x) == "function" then return str.eman(x).."()" end
  if type(x) ~= "table"    then return tostring(x) end
  if type(x) == "number"   then return tostring(math.rnd(x, n or 2)) end 
  t = list.map(x, function(k,v,     x1) 
                    if tostring(k):sub(1,1) ~= "_" then 
                      return #x>0 and str.o(v,n) or str.fmt(":%s %s", k, x1) end end)
  return (t._name or "").. "{" .. str.cat(#x==0 and sort(t) or t," ") .. "}" end

function str.oo(x,  n) 
  print(str.o(x,  n)); return x end
-------------------- ------------------- --------------------- -------------------- ----------
return {lint=lint, list=list,         maths=maths, obj=obj,
        rand=rand, settings=settings, str=sr,      test=test} 
