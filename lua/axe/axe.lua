local this = {
  synopois="axe: optimization = cluster + contrast",
  author  ="Tim Menzies, timm@ieee.org",
  license ="MIT",
  year    = 2020}

-----
-- ## Things
local thing, col  = {ako="thing"},{ako="col"}
local lib,num,sym = {ako="lib"},{ako="num"},  {ako="sym"}
local all         = {ako="all"}

do  local id=0
    function thing.new() id=id+1; return {Is=thing,Id=id} end
end

-----
-- ## Verbs
function all.add(i, ...) return i.Is.add(i, ...) end

-----
-- ## Summaries
function col.new(txt,pos,    i) 
  i = thing.new()
  i.Is = col
  i.pos=pos
  i.txt=txt
  i.w  = txt:find("<") and -1 or 1
  return i end

function sym.new(txt,pos,    i) 
  i = col.new(txt,pos)
  i.Is = sym
  i.seen={}
  i.most, i.mode = 0, nil
  i.mu=0
  return i end

function sym.add(i, x,y)
  if (x ~= "?") then
    i.seen[x] = (i.seen[x] or 0) + 1
    if i.seen[x] > i.most then i.most, i.mode = i.seen[x], x end end
  return x end 

function num.new(txt,pos,    i) 
  i = col.new(txt,pos)
  i.Is = num
  i.mu,i.m2=0,0 
  i.lo = math.maxinteger
  i.hi = math.mininteger
  return i end

function num.add(i,x)
  if (x ~= "?") then
    i.lo = math.min(i.lo,x)
    i.hi = math.max(i.hi,x) end
  return x end

---------
-- ## Lib

-- Iterate on keys in sorted order
function lib.order(t,  i,keys)
  i,keys = 0,{}
  for key,_ in pairs(t) do keys[#keys+1] = key end
  table.sort(keys)
  return function ()
    if i < #keys then
      i=i+1; return keys[i], t[keys[i]] end end end 

-- Simple print of a flat table
function lib.o(z,pre,   s,c) 
  s, c = (pre or "")..'{', ""
  for _,v in lib.order(z or {}) do s= s..c..tostring(v); c=", " end
  return s..'}'
end

-- Nested print of tables. 
-- - Don't show private slots (those that start with "_"),
-- - Show slots in sorted order.  If `pre` is specified, 
--   then  use that as a prefix.
-- - Don't loop if we  hit the same object more than once 
--   (instead,  print "...").
function lib.oo(t,pre,    indent,fmt)
  pre    = pre or ""
  indent = indent or 0
  if(indent==0) then print("") end
  if indent < 10 then
    for k, v in lib.order(t or {}) do
      if not (type(k)=='string' and k:match("^_")) then
        if not (type(v)=='function') then
          fmt = pre..string.rep("|  ",indent)..tostring(k)..": "
          if type(v) == "table" then
            print(fmt)
            lib.oo(v, pre, indent+1)
          else
            print(fmt .. tostring(v)) end end end end end end

-- Warn about locals that have escaped into the global space
function lib.rogues(    ignore,match)
  ignore = {
    jit=true, utf8=true,math=true, package=true, table=true, 
    coroutine=true, bit=true, os=true, io=true, 
    bit32=true, string=true, arg=true, debug=true, 
    _VERSION=true, _G=true }
  for k,v in pairs( _G ) do
    if type(v) ~= "function" and not ignore[k] then
       if k:match("^[^A-Z]") then
         print("-- warning, rogue local ["..k.."]") 
  end end end 
end 

-------
-- ## Return
return {this=this, lib=lib,all=all, num=num, sym=sym}
