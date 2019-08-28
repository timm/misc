#!/usr/bin/env lua
-- vim: paste filetype=lua nospell ts=2 sw=2 sts=2 et :
---------- --------- --------- --------- --------- --

Object = {}

do
  local id = 0
  function Object.new(t)
    t = t or {}
    t.ako = t.ako or "Object"
    id = id + 1
    t._isa= _ENV[t.ako]
    t.id = id
    return t end
end

function abs(x) return x<0 and -1*x or x end

function ordered(t,  i,keys)
  i,keys = 0,{}
  for key,_ in pairs(t) do keys[#keys+1] = key end
  table.sort(keys)
  return function ()
    if i < #keys then
      i=i+1; return keys[i], t[keys[i]] end end
end

function o(t,    pre,   s)
  pre = pre or 0
  for k, v in ordered(t) do
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

function abs(x) return x<0 and -1*x or x end


o{bb=1,aa=2,c={10,20,30}}
rogues()

function libmain()
  x=Object.new()
end
return {main=libmain}
