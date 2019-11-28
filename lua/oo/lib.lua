-- vim: nospell:sta:et:sw=2:ts=2:sts=2

function same(x)  return x end
function fmt(...) return string.format(...) end

local function collect(t,f)
  local out={}
  if t then  
    for i,v in pairs(t) do out[i] = f(v) end end
  return out
end 

function copy(t) 
  return type(t) ~= 'table' and t or collect(t,copy) end

function o(data)  print(""); print(oo(data)) end

function oo(data, indent)  -- convert anything to a string
  if(indent == nil) then indent = 0 end 
  local s,pad = "",string.rep(" ",indent)
  if type(data) ~= "table" then 
    return pad ..tostring(data)
  end
  for i, v in ordered(data) do 
    s = s .. pad .. i .. ": "
    if(type(v) == "table") then 
      s = s .." \n"  .. oo(v,indent+2)
    else 
      s = s .. oo(v,0) .. "\n"
  end end
  return s
end

Object={}

function Object:s()
  return oo(self)
end

function Object:new(o)
   o = o or {} 
   setmetatable(o,self)  
   self.__index = self
   self.__tostring =  o.s
   return o
end

function ordered(t,  i,keys)
  i,keys = 0,{}
  for key,_ in pairs(t) do keys[#keys+1] = key end
  table.sort(keys)
  return function ()
    if i < #keys then
      i=i+1; return keys[i], t[keys[i]] end end
end

function o(t,    pre,   s,out)
  pre = pre or 0
  out = ""
  for k, v in ordered(t) do --ordered(t) do
    if not (type(k)=='string' and k:match("^_")) then
      s = string.rep("|  ", pre) .. k .. ": "
      if type(v) == "table" then
        print(s)
        o(v, pre+1)
      else
        print(s .. tostring(v)) end end end
end

function rogues(    ignore)
  ignore = {jit=true, utf8=true, math=true, package=true,
            table=true, coroutine=true, bit=true, os=true,
            io=true, bit32=true, string=true, arg=true,
            debug=true, _VERSION=true, _G=true }
  for k,v in pairs( _G ) do
   if type(v) ~= "function" and not ignore[k] then
    if k:match("^[^A-Z]") then
     print("-- warning, rogue local ["..k.."]") end end end
end

do 
  local y,n = 0,0
  function ok(t,  n,score,      passed,err,s)
    s=function() return math.floor(0.5+100*(1-((y-n)/y))) end
    for x,f in pairs(t) do
      y = y + 1
      print("-- Test #" .. y ..
            " (oops=".. s() .."%). Checking ".. x .."... ")
      passed,err = pcall(f)
      if not passed then
        n = n + 1
        print("-- E> Failure " .. n .. " of "
              .. y ..": ".. err) end end
    rogues()
  end
end
