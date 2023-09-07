local M={}
function M.map(t,fun,    u) u={}; for k,v in pairs(t) do u[k] = fun(v) end; return u end

function M.trim(s)  return s:match'^%s*(.*%S)' or '' end

function M.coerce(s)
  s=M.trim(s) 
  return math.tointeger(s) or tonumber(s) or s=="true" or (s ~= "false" and s) end

function M.coerces(s,    t)
  t={}; for s1 in s:gmatch("([^,]+)") do t[1+#t]=M.coerce(s1) end; return t end

function M.csv(sFilename,fun,     src,s)
  src = io.input(sFilename)
  while true do
    s = io.read(); if s then fun(M.coerces(s)) else return io.close(src) end end end

function M.settings(s)
  t={_help=s}
  s:gsub("\n[%s]+[-][%S][%s]+[-][-]([%S]+)[^\n]+= ([%S]+)",function(k,v) t[k]=M.coerce(v) end)
  return t end

function M.cli(t)
  for k,v in pairs(t,    s) do
    s=tostring(v)
    for n,x in ipairs(arg) do
      if x=="-"..(k:sub(1,1)) or x=="--"..k then
        t[k]= M.coerce(s=="false" and "true" or s=="true" and "false" or arg[n+1]) end end end 
  if t.help then print(t._help) end
  return t end

function M.o(x,  n,    t)
  if type(x) == "function" then return "f()" end
  if type(x) ~= "table"    then return tostring(x) end
  if type(x) == "number"   then return M.rnd(x,d==nil and 2 or d) end
  t={}; for k,v in pairs(x) do 
          if tostring(k):sub(1,1) ~= "_" then 
            t[1+#t] = #x>0 and M.o(v,n) or string.format(":%s %s",k,M.o(v,n)) end end 
  if #x==0 then table.sort(t) end
  return "{"..table.concat(t," ").."}" end
  
function M.oo(x,  n) print(M.o(x,  n)); return x end

function M.rnd(num, numDecimalPlaces,    mult)
  mult = 10^(numDecimalPlaces or 0)
  return math.floor(num * mult + 0.5) / mult end

return M
