local l={}

function l.per(t,p) return t[(p*#t)//1] end
function l.mid(t)   return l.per(t,.5) end
function l.div(t)   return (l.per(t,.9) - l.per(t,.1))/2.56 end

function l.push(t,x) t[1+#t]=x; return x end

function l.cat(t,     u)
  u={}; for k,v in pairs(t) do u[k]=tostring(v) end
  return table.concat(u,", ") end

function l.map(t,fun,...) --> t
  local u={};  for k,v in pairs(t) do u[1+#u] = fun(v,...) end; return u end

function l.kap(t,fun,...) --> t
  local u = {}; for k, v in pairs(t) do
                  u[1+#u] = fun(k,v,...) end; return u end

l.fmt = string.format

function l.rnd(n, ndecs)  
  if type(n) ~= "number" then return n end
  if math.floor(n) == n  then return n end
  local mult = 10^(ndecs or 3)
  return math.floor(n * mult + 0.5) / mult end

function l.oo(any,  ndecs)  
  print(l.o(any,ndecs)); return any end

function l.o(any,  ndecs,     fun, u)  
  function fun(k, v)
    k = tostring(k)
    if not k:find "^_" then
      return l.fmt(":%s %s", k, l.o(v, ndecs)) end end
  if type(any) == "number" then return tostring(l.rnd(any,ndecs)) end
  if type(any) ~= "table" then return tostring(any) end
  u = #any == 0 and l.sort(l.kap(any, fun)) or l.map(any, l.o, ndecs)
  return "{"..table.concat(u,", ").."}" end 

function l.sort(t,  fun) --> t
  table.sort(t,fun); return t end

function l.coerce(s,    fun)
  function fun(s)
    if s=="nil" then return nil
    else return s=="true" or (s~="false" and s) end end
  return math.tointeger(s) or tonumber(s) or fun(s:match'^%s*(.*%S)') end

function l.cells(s1,    t)
  t={}; for s2 in s1:gmatch("([^,]+)") do t[1+#t]=l.coerce(s2) end; 
  return t end

function l.csv(src,fun,    line,nr)
  src =  src=="" and io.input() or io.input(src)
  nr,line = -1,io.read()
  while line do
    nr=nr+1
    fun(nr,line)
    line = io.read() end
  io.close(src) end

function l.cli(t) 
  for k,v in pairs(t) do
    v = tostring(v)
    for n,x in ipairs(arg) do
      if x=="-"..(k:sub(1,1)) or x=="--"..k then
        v= ((v=="false" and "true") or (v=="true" and "false") or arg[n+1])
        t[k] = l.coerce(v) end end end
  return t end
    
return l