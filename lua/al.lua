big  = 1E32
fmt  = string.format
atom = function(s) return tonumber(s) or s:match"^%s*(.-)%s*$" end
new  = function(kl,t) kl.__index=kl; return setmetatable(t,kl) end

function map(t,fn,    u) 
  u={}; for _,v in pairs(t) do u[1+#u] = fn(v) end; return u end

function cells(s,fn,    t)
  t={}; for s1 in s:gmatch("([^,]+)") do t[1+#t]=fn(s1) end; return t end

function o(x,      t,_arr,_dic,_num)
  t   = {}
  _arr=function() for _,v in pairs(x) do t[1+#t]=o(v) end end
  _dic=function() for k,v in pairs(x) do t[1+#t]=fmt(":%s %s",k,o(v))end end
  _num=function() return x//1 == x and "%s" or "%.3g" end
  if type(x) == "number" then return fmt(_num(), x) end
  if type(x) ~= "table"  then return tostring(x) end
  if #x>0 then _arr() else _dic(); table.sort(t) end
  return "{" .. table.concat(t, " ") .. "}" end

function csv(s,fn,    src,s)
  src = io.input(s)
  s = io.read()
  while s do print(s); fn(cells(s, atom)); s=io.read() end
  io.close(src) end


