local l={}
local b4={}; for k,_ in pairs(_ENV) do  b4[k]=k end

-- ## lint ----------------------------------------------------------
function l.rogues() --> nil
  for k,v in pairs(_ENV) do if not b4[k] then print("?",k,type(v)) end end end

-- ## lists ----------------------------------------------------------
function l.map(t,fun,...) --> t
  local u={}; for k,v in pairs(t) do u[1+#u] = fun(v,...) end; return u end

function l.kap(t,fun,...) --> t
    local u = {}; for k, v in pairs(t) do
                    u[1+#u] = fun(k,v,...) end; return u end

function l.sort(t,  fun) --> t
  table.sort(t,fun); return t end

function l.copy(t,    u) --> any
  if type(t) ~= "table" then return t end
  u={}; setmetatable(u, getmetatable(t))
  for k,v in pairs(t) do u[l.copy(k)] = l.copy(v) end
  return u end

function l.items(t,fun,    u,i) --> fun --> (k,any)
  u={}; for k,_ in pairs(t) do u[1+#u]=k end
  table.sort(t,fun)
  i=0
  return function()
    if i<#u then i=i+1; return u[i], t[u[i]] end end end 

function l.report(ts, nwidth)
  s = "%"..(nwidth or 4).."s"
  function v(k1,v1) return fmt(s,v1) end
  function k(k1,v1) return fmt(s,k1) end
  print(table.concat(kap(ts[1],k),", "))
  for k,t in pairs(ts) do print(table.contact(kap(t,v))..fmt(s,k)) end end
  
function l.asList(t,    u) --> t
  u={}; for k,v in pairs(t) do u[1+#u] = v end; return v end

-- ## maths string ----------------------------------------------------------
function l.rnd(n, ndecs) --> num. return `n` rounded to `nPlaces`
  if type(n) ~= "number" then return n end
  if math.floor(n) == n  then return n end
  local mult = 10^(ndecs or 3)
  return math.floor(n * mult + 0.5) / mult end

-- ## to string ----------------------------------------------------------
l.fmt = string.format

function l.oo(any,  ndecs) --> any
  print(l.o(any,decs)); return any end

function l.o(any,  ndecs,       fun, u) --> s
  function fun(k, v)
    k = tostring(k)
    if not k:find "^_" then
      return l.fmt(":%s %s", k, l.o(v, ndecs)) end end
  if type(any) == "number" then return tostring(l.rnd(any,decs)) end
  if type(any) ~= "table" then return tostring(any) end
  u = #any == 0 and l.sort(l.kap(any, fun)) or l.map(any, l.o, decs)
  return "{"..table.concat(u,", ").."}" end 

-- ## from string --------------------------------------------------------
function l.coerce(s,    fun) --> string | bool | float | int | nil
  function fun(s) 
    if   s=="nil" then return nil 
    else return s=="true" or (s~="false" and s) end end
  return math.tointeger(s) or tonumber(s) or fun(s:match'^%s*(.*%S)') end

function l.csv(s,  n,src) --> fun --> (num,t)
  if s=="-" then s=nil end
  n, src = -1, io.input(s)
  return function(    s,t)
    s1 = io.read()
    if   s1
    then n=n+1
         t={}; for s2 in s1:gmatch("([^,]+)") do t[1+#t]=l.coerce(s2) end
         return n,t
    else io.close(src) end end end

-- ## settings -----------------------------------------------
function l.settings(s,    t,pat) --> t
  t={_help=s}
  pat = "\n[%s]+[-][%S][%s]+[-][-]([%S]+)[^\n]+= ([%S]+)"
  for k,s1 in s:gmatch(pat) do t[k]= l.coerce(s1) end
  return t end

function l.cli(t) --> t
  for k,v in pairs(t) do
    v = tostring(v)
    for n,x in ipairs(arg) do
      if x=="-"..(k:sub(1,1)) or x=="--"..k then
        v= ((v=="false" and "true") or (v=="true" and "false") or arg[n+1])
        t[k] = l.coerce(v) end end end
  if t.help then os.exit(print("\n"..t._help)) end
  return t end

-- ## tests and demos ---------------------------------------
function l.try(s, tsettings,fun,       b4,oops) --> bool
  b4 = l.copy(tsettings)
  math.randomseed(tsettings.seed or 1234567891)
  io.write("🔷 ".. s.." ")
  oops= fun()==false 
  for k,v in pairs(b4) do tsettings[k]= v end
  if   oops
  then print(" ❌ FAIL"); return true
  else print("✅ PASS");  return false end  end

function l.run(tsettings,funs) --> nil
  l.cli(tsettings)
  for _,com in pairs(arg) do 
    if funs[com] then l.try(com, tsettings, funs[com]) end end 
  l.rogues() end

function l.runall(tsettings, funs,      oops) --> nil
  oops = -1 -- we have one test that deliberately fails
    for k, fun in l.items(funs) do
    print("\n"..k)
    if k~="all" then 
      if l.try(k,tsettings,fun) then oops = oops + 1 end end end
  l.rogues()
  os.exit(oops) end

return l
