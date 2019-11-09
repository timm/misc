-- class.lua
-- Compatible with Lua 5.1 (not 5.0).
-- http://lua-users.org/wiki/SimpleLuaClasses

function class(base, init)
   local c = {}    -- a new class instance
   if not init and type(base) == 'function' then
      init = base
      base = nil
   elseif type(base) == 'table' then
    -- our new class is a shallow copy of the base class!
      for i,v in pairs(base) do
         c[i] = v
      end
      c._base = base
   end
   -- the class will be the metatable for all its objects,
   -- and they will look up their methods in it.
   c.__index = c

   -- expose a constructor which can be called by <classname>(<args>)
   local mt = {}
   mt.__call = function(class_tbl, ...)
     local obj = {}
     setmetatable(obj,c)
     if class_tbl.init then
        class_tbl.init(obj,...)
     else 
        -- make sure that any stuff from the base class is initialized!
        if base and base.init then
        base.init(obj, ...)
        end
     end
     return obj
   end
   c.init = init
   c.is_a = function(self, klass)
      local m = getmetatable(self)
      while m do 
         if m == klass then return true end
         m = m._base
      end
      return false
   end
   setmetatable(c, mt)
   return c
end

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

