local int,fmt = math.tointeger,string.format
local the,help = {},[[
  -a apple=2.23222 asdas
  -s seed=1 random number seed
  -c card=4 asdas ]]

local function cast(s) return int(s) or tonumber(s)or s:match"^%s*(.-)%s*$" end

local function order(t,     keys,i)
  if #t>0 then return ipairs(t) end; 
  u,i = {},0
  for k in pairs(t) do u[#u+1]=k end; table.sort(u)
  return function() i=i+1; if u[i] then return u[i],t[u[i]] end end end

local function o(t,     u)
  if math.type(t)=="float" then return fmt("%.2f",t) end
  if type(t)~="table" then return tostring(t) end
  u={}; for k,v in order(t) do u[#u+1]= #t>0 and o(v) or fmt(":%s %s",k,o(v)) end
  return "{"..table.concat(u," ").."}" end

for k,v in help:gmatch("(%S+)=(%S+)") do the[k] = cast(v) end
math.randomseed(the.seed)
print(o(the))
