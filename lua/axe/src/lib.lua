-- <img src="https://image.flaticon.com/icons/png/512/264/264745.png" width=75">   
-- <a href="http://github.com/tomm/keys"><img src="https://github.blog/wp-content/uploads/2008/12/forkme_left_red_aa0000.png?resize=149%2C149" align=left></a>  
-- "Keys = cluster, discretize, contrast"
-- ![](https://img.shields.io/badge/platform-osx%20,%20linux-lightgrey?style=flat-square)  
-- ![](https://img.shields.io/badge/language-lua,bash-blue?style=flat-square)  
-- ![](https://img.shields.io/badge/purpose-ai%20,%20se-blueviolet?style=flat-square)  
-- ![](https://img.shields.io/badge/language-lua-red?style=flat-square)  
-- ![](https://img.shields.io/badge/license-mit-green?style=flat-square)  
-- [lib](lib.html) :: [tbl](tbl.html)   
--------------------
local Of  ={
  synopois= "Misc lua routines",
  author  = "Tim Menzies, timm@ieee.org",
  license = "MIT",
  year    = 2020 }

-- Return any item in a list
local function any(a) return a[1 + math.floor(#a*math.random())] end

-- Split the string `s` on separator `c`, defaults to "." 
local function split(s,     c,t)
  t, c = {}, c or ","
  for y in string.gmatch(s, "([^" ..c.. "]+)") do t[#t+1] = y end
  return t end

-- Deep copy
local function copy(obj,   old,new)
  if type(obj) ~= 'table' then return obj end
  if old and old[obj] then return old[obj] end
  old, new = old or {}, {}
  old[obj] = new
  for k, v in pairs(obj) do new[k] = copy(v, old) end
  return new end

-- Object creation, add a unique id, bind to metatable, maybe set some initial values.
local id=0
local function isa(klass,inits,      new)
  new = copy(klass or {})
  for k,v in pairs(inits or {}) do new[k] = v end
  setmetatable(new, klass)
  klass.__index = klass
  id = id + 1
  new.id = id
  return new end 

-- Iterate on keys in sorted order
local function order(t,  i,keys)
  i,keys = 0,{}
  for key,_ in pairs(t) do keys[#keys+1] = key end
  table.sort(keys)
  return function ()
    if i < #keys then
      i=i+1; return keys[i], t[keys[i]] end end end 

-- Simple print of a flat table
local function o(z,pre,   s,c) 
  s, c = (pre or "")..'{', ""
  for _,v in order(z or {}) do s= s..c..tostring(v); c=", " end
  print(s..'}') end

-- Print nested tables. 
-- Don't show private slots (those that start with `_`);
-- show slots in sorted order;
-- if `pre` is specified, then  print that as a prefix.
local function oo(t,pre,    indent,fmt)
  pre    = pre or ""
  indent = indent or 0
  if(indent==0) then print("") end
  if indent < 10 then
    for k, v in order(t or {}) do
      if not (type(k)=='string' and k:match("^_")) then
        if not (type(v)=='function') then
          fmt = pre..string.rep("|  ",indent)..tostring(k)..": "
          if type(v) == "table" then
            print(fmt)
            oo(v, pre, indent+1)
          else
            print(fmt .. tostring(v)) end end end end end end

-- Warn about locals that have escaped into the global space
function rogues(    ignore,match)
  ignore = {
    jit=true, utf8=true,math=true, package=true, table=true, 
    coroutine=true, bit=true, os=true, io=true, 
    bit32=true, string=true, arg=true, debug=true, 
    _VERSION=true, _G=true }
  for k,v in pairs( _G ) do
    if type(v) ~= "function" and not ignore[k] then
       if k:match("^[^A-Z]") then
         print("-- warning, rogue local ["..k.."]") end end end end 

-- Return each row, split on ",", numstrings coerced to numbers,
-- kills comments and whitespace.
function csv(file,     stream,tmp,t)
  stream = file and io.input(file) or io.input()
  tmp    = io.read()
  return function()
    if tmp then
      tmp = tmp:gsub("[\t\r ]*","") -- no whitespace
               :gsub("#.*","") -- no comemnts
      t   = split(tmp) 
      tmp = io.read()
      if #t > 0 then 
        for j,x in pairs(t) do t[j] = tonumber(x) or x end
        return t end
    else
      io.close(stream) end end end

-----
-- Any finally...
return {any=any, split=split, copy=copy, rogues=rogues,
        csv=csv, isa=isa, order=order, o=o, oo=oo}

