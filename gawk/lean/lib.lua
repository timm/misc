local l={}
local b4={}; for k,_ in pairs(_ENV) do  b4[k]=k end

-- ## lint ----------------------------------------------------------
function l.rogues()
  for k,v in pairs(_ENV) do if not b4[k] then print("?",k,type(v)) end end end

-- ## lists ----------------------------------------------------------
function l.map(t,fun,...)
  local u={}; for k,v in pairs(t) do u[1+#u] = fun(v,...) end; return u end

function l.kap(t,fun,...)
  local u={}; for k,v in pairs(t) do u[1+#u] = fun(k,v,...) end; return u end

function l.sort(t,fun) table.sort(t,fun); return t end

function l.copy(t,    u)
  if type(t) ~= "table" then return t end
  u={}; setmetatable(u, getmetatable(t))
  for k,v in pairs(t) do u[l.copy(k)] = l.copy(v) end
  return u end

function l.items(t,fun,    u,i)
  u={}; for k,_ in pairs(t) do u[1+#u]=k end
  table.sort(t,fun)
  i=0
  return function()
    if i<#u then i=i+1; return u[i], t[u[i]] end end end 

-- ## maths string ----------------------------------------------------------
function l.rnd(n, decs) --> num. return `n` rounded to `nPlaces`
  if type(n) ~= "number" then return n end
  if math.floor(n) == n  then return n end
  local mult = 10^(decs or 3)
  return math.floor(n * mult + 0.5) / mult end

-- ## to string ----------------------------------------------------------
l.fmt = string.format

function l.oo(x,  decs) print(l.o(x,decs)); return x end

function l.o(x,  decs,       kv, u)
  function kv(k, v) if not k:find"^_" then return l.fmt(":%s %s", k, l.o(v, decs)) end end
  if type(x) == "number" then return tostring(l.rnd(x,decs)) end
  if type(x) ~= "table" then return tostring(x) end
  u = #x == 0 and l.sort(l.kap(x, kv)) or l.map(x, l.o, decs)
  return "{"..table.concat(u,", ").."}" end 

-- ## from string --------------------------------------------------------
function l.coerce(s,    fun)
  function fun(s) 
    if   s=="nil" then return nil 
    else return s=="true" or (s~="false" and s) end end
  return math.tointeger(s) or tonumber(s) or fun(s:match'^%s*(.*%S)') end

function l.csv(src,  n)
  if src=="-" then src=nil end
  n, src = -1, io.input(src)
  return function(    s,t)
    s = io.read()
    if   s
    then n=n+1
         t={}; for s1 in s:gmatch("([^,]+)") do t[1+#t]=l.coerce(s1) end
         return n,t
    else io.close(src) end end end

-- ## settings -----------------------------------------------
function l.settings(s,    t,pat)
  t={_help=s}
  pat = "\n[%s]+[-][%S][%s]+[-][-]([%S]+)[^\n]+= ([%S]+)"
  for k,s1 in s:gmatch(pat) do t[k]= l.coerce(s1) end
  return t end

function l.cli(t)
  for k,v in pairs(t) do
    v = tostring(v)
    for n,x in ipairs(arg) do
      if x=="-"..(k:sub(1,1)) or x=="--"..k then
        v= ((v=="false" and "true") or (v=="true" and "false") or arg[n+1])
        t[k] = l.coerce(v) end end end
  if t.help then os.exit(print("\n"..t._help)) end
  return t end

-- ## tests and demos ---------------------------------------
function l.try(s, settings,fun,       b4,oops)
  b4 = l.copy(settings)
  math.randomseed(settings.seed or 1234567891)
  io.write("🔷 ".. s.." ")
  oops= fun()==false 
  for k,v in pairs(b4) do settings[k]= v end
  if   oops
  then print(" ❌ FAIL"); return true
  else print("✅ PASS");  return false end  end

function l.run(settings,eg)
  l.cli(settings)
  for _,com in pairs(arg) do 
    if eg[com] then l.try(com, settings, eg[com]) end end 
  l.rogues() end

function l.runall(settings, eg,      oops)
  oops = -1 -- we have one test that deliberately fails
    for k, fun in l.items(eg) do
    print("\n"..k)
    if k~="all" then 
      if l.try(k,settings,fun) then oops = oops + 1 end end end
  l.rogues()
  os.exit(oops) end

return l