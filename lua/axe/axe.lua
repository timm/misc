local of = {
  synopois= "axe: optimization = cluster + contrast",
  author  = "Tim Menzies, timm@ieee.org",
  license = "MIT",
  year    = 2020,
  seed    = 1,
  ch      = {skip="?", more=">", less="<"}
}

-- ## Things
local thing, col  = {ako="thing"},{ako="col"}
local lib,num,sym = {ako="lib"}, {ako="num"},  {ako="sym"}
local skip        = {ako="skip"}
local row,rows    = {ako="row"},  {ako="rows"}
local _
-- All `thing`s have a unique id.
do local id=0
   function thing.new() id=id+1; return {Is=thing,Id=id} end end

-- ## Verbs
local function add(i, ...) return i.It.add(i, ...) end

-- ## Column summaries
_ = col
function _.new(pos,txt,    i)  
  i = thing.new()
  i.Is = col
  i.pos=pos
  i.txt=txt
  i.w  = txt:find(of.ch.less) and -1 or 1
  return i end

function _.what(n,s) 
  local tmp = s:find(of.ch.skip) and skip or (
              s:find(of.ch.sym)  and sym or num) 
  return tmp.new(n,s) end

_=sym
function _.new(pos,txt,    i) 
  i = col.new(pos,txt)
  i.Is = col
  i.seen={}
  i.most, i.mode = 0, nil
  i.mu=0
  return i end

function _.add(i, x)
  if (x ~= of.ch.skip) then
    i.seen[x] = (i.seen[x] or 0) + 1
    if i.seen[x] > i.most then i.most, i.mode = i.seen[x], x end end
  return x end 

_=skip
function _.new(pos,txt,    i) 
  i    = col.new(pos,txt)
  i.Is = skip 
  return i end

function _.add(i, x) return x end

_=num
function _.new(pos,txt,    i) 
  i = col.new(pos,txt)
  i.Is = num
  i.mu,i.m2=0,0 
  i.lo = math.maxinteger
  i.hi = math.mininteger
  return i end

function _.add(i,x)
  if (x ~= of.ch.skip) then
    i.lo = math.min(i.lo,x)
    i.hi = math.max(i.hi,x) end
  return x end

_=num
function _.new(cols,     i)
  i    = thing.new()
  i.Is = num
  i.cells, i.bins ={}, {}
  for _,c in pairs(cols) do 
    i.cells[c] = add(t[c.pos],t[c]) 
    i.bins[c]  = i.cells[c] end 
  return i end

-- ## Container for many rows
-- Summaries in columns (see `i.cols`).

_=rows
function _.new(i)    
  i    = thing.new()
  s.Is = rows
  i.n  = 0
  i.cols, i.rows = {},{} 
  return i end

function _.add(i, t)
  return #i.cols==0 and _.head(i,t) or _.data(i,t) end

function _.data(i,t) 
  i.n = i.n + 1
  i.rows[math.floor(10^9 * math.random())] = row.new(t,cols) end
  
function _.head(i,t)
  for n,s in pairs(t) do i.cols[j] = col.what(n,s) end end

function _.read(i,f) 
  for row in lib.csv(f) do _.add(i, row) end
  return i end

-- ## Lib

-- Polymorphism (one ring to rule them all)
function lib.go(i,f, ...) return i.Is[f](i, ...) end

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
  return s..'}' end

-- Nested print of tables. 
-- Don't show private slots (those that start upper case),
-- Show slots in sorted order.
-- If `pre` is specified, then  print that as a prefix.
function lib.oo(t,pre,    indent,fmt)
  pre    = pre or ""
  indent = indent or 0
  if(indent==0) then print("") end
  if indent < 10 then
    for k, v in lib.order(t or {}) do
      if not (type(k)=='string' and k:match("^[A-Z]")) then
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
         print("-- warning, rogue local ["..k.."]") end end end end 

-- Return each row, split on ",", numstrings coerced to numbers,
-- kills comments and whitespace.
function lib.csv(file,     stream,tmp,t)
  stream = file and io.input(file) or io.input()
  tmp    = io.read()
  return function()
    if tmp then
      t   = lib.split(tmp:gsub("([\t\r ]*|#.*)","")) -- no whitespace
      tmp = io.read()
      if #t > 0 then 
        for j,x in pairs(t) do t[j] = tonumber(t[j]) or t[j] end
        return t end
    else
      io.close(stream) end end end

--  Split the string `s` on separator `c`, defaults to "." 
function lib.split(s,     c,t)
  t, c = {}, c or ","
  for y in string.gmatch(s, "([^" ..c.. "]+)") do t[#t+1] = y end
  return t end


-- ## Return
return {of=of, lib=lib,num=num, sym=sym,rows=row}
