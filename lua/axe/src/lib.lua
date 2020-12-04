
-- Misc library routines
local Lib={}

-- return any item in a list
function Lib.any(a) return a[1 + math.floor(#a*math.random())] end

-- Split the string `s` on separator `c`, defaults to "." 
function Lib.split(s,     c,t)
  t, c = {}, c or ","
  for y in string.gmatch(s, "([^" ..c.. "]+)") do t[#t+1] = y end
  return t end

-- Deep copy
function Lib.copy(obj,   old,new)
  if type(obj) ~= 'table' then return obj end
  if old and old[obj] then return old[obj] end
  old, new = old or {}, {}
  old[obj] = new
  for k, v in pairs(obj) do new[k]=Lib.copy(v, old) end
  return new end

-- Object creation
do
  local id=0
  function Lib.isa(klass,has,      new)
    new = Lib.copy(klass or {})
    for k,v in pairs(has or {}) do new[k] = v end
    setmetatable(new, klass)
    klass.__index = klass
    id = id + 1
    new.id = id
    return new end end

-- Iterate on keys in sorted order
function Lib.order(t,  i,keys)
  i,keys = 0,{}
  for key,_ in pairs(t) do keys[#keys+1] = key end
  table.sort(keys)
  return function ()
    if i < #keys then
      i=i+1; return keys[i], t[keys[i]] end end end 

-- Simple print of a flat table
function Lib.o(z,pre,   s,c) 
  s, c = (pre or "")..'{', ""
  for _,v in Lib.order(z or {}) do s= s..c..tostring(v); c=", " end
  print(s..'}') end

-- Print nested tables. 
-- Don't show private slots (those that start with `_`);
-- show slots in sorted order;
-- if `pre` is specified, then  print that as a prefix.
function Lib.oo(t,pre,    indent,fmt)
  pre    = pre or ""
  indent = indent or 0
  if(indent==0) then print("") end
  if indent < 10 then
    for k, v in Lib.order(t or {}) do
      if not (type(k)=='string' and k:match("^_")) then
        if not (type(v)=='function') then
          fmt = pre..string.rep("|  ",indent)..tostring(k)..": "
          if type(v) == "table" then
            print(fmt)
            Lib.oo(v, pre, indent+1)
          else
            print(fmt .. tostring(v)) end end end end end end

-- Return
return Lib

