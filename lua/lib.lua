--  ___              __        
-- /\_ \      __    /\ \       
-- \//\ \    /\_\   \ \ \____  
--   \ \ \   \/\ \   \ \ '__`\ 
--    \_\ \_  \ \ \   \ \ \L\ \
--    /\____\  \ \_\   \ \_,__/
--    \/____/   \/_/    \/___/ 
--
                            

local l={}

l.big  = 1E32
l.fmt  = string.format
l.push = function(t,x)     t[1+#t]=x; return x                              end
l.oo   = function(t)       print(l.o(t)); return t                          end
l.any  = function(t)       return t[math.random(#t)]                        end
l.sort = function(t,fn)    table.sort(t,fn); return t                       end
l.new  = function(kl,t)    kl.__index=kl; return setmetatable(t,kl)         end
l.many = function(t,n,  u) u={}; for _ =1,n do u[1+#u]=any(t) end; return u end

function l.map(t,fn,    u)
  u={}; for _,v in pairs(t) do u[1+#u] = fn(v) end; return u end

function l.sum(t,fn,    n)
  n=0; for _,v in pairs(t) do n = n + fn(v) end; return n end

function l.most(t,fn,    m,n,x)
  n = -l.big
  for _,v in pairs(t) do m=fn(v); if m>n then n,x=m,v end end; return x end

--### Strings to Things
function l.atom(s,    fn)
  function fn(s1) return s1=="true" or s1~="false" and s1 end
  return math.tointeger(s) or tonumber(s) or fn(s:match"^%s*(.-)%s*$") end

function l.atoms(s,    t)
  t={}; for s1 in s:gmatch("([^,]+)") do t[1+#t]=l.atom(s1) end; return t end

--### Thing to Strings
function l.o(x,      t,_arr,_dic,_num)
  t   = {}
  _arr=function() for _,v in pairs(x) do t[1+#t]=l.o(v) end end
  _dic=function() for k,v in pairs(x) do t[1+#t]=l.fmt(":%s %s",k,l.o(v))end end
  _num=function() return x//1 == x and "%s" or "%.3g" end
  if type(x) == "number" then return l.fmt(_num(), x) end
  if type(x) ~= "table"  then return tostring(x) end
  if #x>0 then _arr() else _dic(); table.sort(t) end
  return "{" .. table.concat(t, " ") .. "}" end

return l

