local the,big,min,max,s2n,s2a,atom,push,sort,map,kap,atom,kap,o,csv

the = {bins = 12,
       p    = 2, 
       seed = 1234567891,
       file = "../../moot/optimize/misc/auto93.csv"}

big  = 1E32
min  = math.min
max  = math.max

trim = function(s) return s:match"^%s*(.-)%s*$" end
s2n  = function(s) return tonumber(s) or math.tointeger(s) end
s2a  = function(s) return (s=="true" and true) or (s~= "false" and s) end
atom = function(s) return s2n(s) or s2a(trim(s)) end

push = function(t,x)  t[1+#t]=x; return x end
sort = function(t,fn) table.sort(t,fn); return t end
map  = function(t,fn) return kap(t,function(_,v) return fn(v) end) end

function kap(t,fn,    u) 
  u={}; for k,v in pairs(t) do u[1+#u] = fn(k,v) end; return u end  

function o(x,     _kv,_fmt)
  _fmt= string.format
  _kv = function(k,v) return _fmt(":%s %s",o(k),o(v)) end
  return type(x)=="number" and _fmt(x//1==x and "%s" or "%.3g", x) or 
         type(x)~="table"  and tostring(x) or
         "{"..table.concat(#x>0 and map(x,o) or sort(kap(x,_kv)),", ").."}" end

function csv(file,     src,_atoms) 
  _atoms= function(s,    t) 
    t={}; for x in s:gmatch("([^,]+)") do t[1+#t]=atom(x) end; return t end

  src = io.input(file)
  return function(    s) 
    s = io.read()
    if s then return _atoms(s) else io.close(src) end end end

-------------------------------------------------------------------------------
local Data,add,read

Data = function() return {rows={}, names={}, x={}, y={}, hi={},lo={}} end

function add(d,t)
  _meta= function()
    d.names = t
    for c,s in pairs(t) do
      if s:find"^[A-Z]" then d.lo[c]=big; d.hi[c]= -big end
      if     s:find"-$" then y[c] = 0 
      elseif s:find"+$" then y[c] = 1 
      else   x[c]=1 end end end 
  
  _data= function()
    push(d.rows, t) 
    for c,hi in pairs(d.hi) do
      v=t[c]
      if v ~= "?" then
        d.hi[c] = max(v, hi) 
        d.lo[c] = min(v, d.lo[c]) end end end

  if #data.names > 0 then _data() else _meta() end end

function read(d, file)
  for t in csv(file) do add(d,t) end
  return d end

-------------------------------------------------------------------------------
local eg={}

eg["--the"] = function(_) print(o(the)) end

eg["--csv"] = function(_) 
  for t in csv(the.file) do print(o(t)) end end

-------------------------------------------------------------------------------
if arg[0]:find"??am.lua" then
  for n,s in pairs(arg) do
    if eg[s] then 
      math.randomseed(the.seed)
      eg[s](arg[n+1]) end end end
