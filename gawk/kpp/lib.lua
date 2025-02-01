--     |  o  |_  
--     |  |  |_) 

local l={}

function l.any(t) return t[math.random(#t)] end

function l.cells(s,  z) 
  z={}; for s2 in s:gmatch"([^,]+)" do z[1+#z]=l.coerce(s2) end; return z end

function l.cli(t)
  for k,v in pairs(t) do
    v = tostring(v)
    for n,x in ipairs(arg) do
      if x=="-"..(k:sub(1,1)) or x=="--"..k then
        v= v=="false" and "true" or v=="true" and "false" or arg[n+1] end end 
    t[k] = l.coerce(v) end 
  return t end

function l.coerce(s,     f)
  f = function(s) if s=="true" then return true end
                  if s=="false" then return false end
                  return s end
  return math.tointeger(s) or tonumber(s) or f(l.trim(s)) end

function l.csv(src)
  src = io.input(src)
  return function(      s)
    s = io.read(); if s then return l.cells(s) else io.close(src) end end end

l.fmt=string.format

function l.keysort(a,f)
  local decorate   = function(x) return {f(x),x} end
  local undecorate = function(x) return x[2] end
  return map(sort(map(a, decorate), l.lt(1)), undecorate) end

function l.lt(f) return function(a,b) return f(a) < f(b) end end

function l.map(t,f,    z)
  z={}; for _,x in pairs(t) do z[1+#z] = f(x) end; return z end

function l.min(t,f,    lo,n,z)
  lo = 1E32
  for _,x in pairs(t) do 
    z= z or x
    n=f(x); if n < lo then lo,z = n,x end end
  return z end

function l.new(mt,a) 
  mt.__index = mt
  mt.__tostring = mt.__tostring or l.o
  return setmetatable(a,mt) end

function l.o(x,          t,f,g)
  t= {}
  f= function() for k,v in pairs(x) do t[1+#t]= l.o(v) end end
  g= function() for k,v in pairs(x) do t[1+#t]= l.fmt(":%s %s",k,l.o(v)) end end
  if type(x) == "number" then return l.fmt(x//1 == x and "%s" or "%.3g",x) end
  if type(x) ~= "table"  then return tostring(x) end
  if #x>0 then f() else g(); table.sort(t) end
  return "{" .. table.concat(t, " ") .. "}" end

function l.push(t,x) t[1+#t]=x; return x end

function l.shuffle(t,    u,j)
  u={}
  for _,x in pairs(t) do u[1+#u]=x; end;
  for i = #u,2,-1 do j=math.random(i); u[i],u[j] = u[j],u[i] end
  return u end

function l.sort(t,f,    u)
  u=map(t, function(x) return x end) 
  table.sort(u,f)
  return u end
  
function l.trim(s) return s:match"^%s*(.-)%s*$" end



return l
