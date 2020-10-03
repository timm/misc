-- vim : ft=lua ts=2 sw=2 et:
--[[
GaTE
--]]

function int(x)        return (x+0.5)//1 end
function within(z,x,y) return (z>=x and z<=y) and z or x+z%(y-x) end
function from(x,y)     return x+(y-x)*math.random() end

function keys(t,        i,u)
  i,u = 0,{}
  for k,_ in pairs(t) do u[#u+1] = k end
  table.sort(u)
  return function () 
    if i < #u then 
      i = i+1
      return u[i], t[u[i]] end end 
end 


function oo(t,pre,    indent,fmt)
  pre    = pre or ""
  indent = indent or 0
  if indent < 10 then
    for k, v in keys(t or {}) do
      if not (type(k)=='string' and k:match("^_")) then
        if not (type(v)=='function') then
          fmt = pre..string.rep("|  ",indent)..tostring(k)..": "
          if type(v) == "table" then
            print(fmt)
            oo(v, pre, indent+1)
          else
            print(fmt .. tostring(v)) end end end end end
end

function copy(obj,   old,new)
  if type(obj) ~= 'table' then return obj end
  if old and old[obj] then return old[obj] end
  old, new = old or {}, {}
  old[obj] = new
  for k, v in pairs(obj) do new[copy(k, old)]=copy(v, old) end
  return setmetatable(new, getmetatable(obj))
end


