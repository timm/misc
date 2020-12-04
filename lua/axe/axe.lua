-- <p style="text-align:left;">
-- Repeat the following until happy or bored. 
-- Useful defaults for this algorithm are C,N,X,Z=20,100,20,2.  
-- </p>
-- <ul style="text-align:left;">
-- <li> Create N pairs of <inputs,outputs>  by either selecting 
--      from a database or running a simulator or asking an oracle.  
-- <li> Cluster on the pairs on the output scores into groups of size .5X%. . 
--      If there is only one score, just sort on that one output variable.  
--      Else, cluster the pairs using the output scores with (say) a 
--      recursive KMEANS++ algorithm with k=2.   
-- <li> Using the clusters, divide the N pairs into  the X% best (B) 
--      and Y=100-X% worst (W outputs.   
-- <li> Use something simple like C% equal percentile chops or Chi-merge 
--      to divide numeric inputs into ranges R1, R2, etc.   
-- <li> Take all those ranges, and all the ranges of non-numeric inputs R10,R11,
--      etc and count  how often they appear among the best and worst pairs. 
--      Normalize those counts  as follows: #B=#B/(N*X/100) and #W=#W/(N*Y/100)
-- <li> Discard unpromising ranges; i.e. if  #W >= #B.    
--      Sort the remaining ranges by #B^2/(#B+#W) into a list L of size S
-- <li> Generate N’ new inputs by,  N times, using inputs L[0:max(1,int(S*rand()^Z))]  
--      (and randomly selected items for everything else).  To create the new output 
--      scores either ask some oracle or re-run the simulator (if it exists) or 
--      interpolate between nearest neighbors in the database. 
-- <li> Return the final best X% group.
-- </ul>

local of  = {
  synopois= "axe: optimization = cluster + contrast",
  author  = "Tim Menzies, timm@ieee.org",
  license = "MIT",
  year    = 2020,
  seed    = 1,
  ch      = {skip="?", klass="!",sym="_", num=":", more=">", less="<"}
  keys    = {c=20, n=100, x=20, z=2}
}

----------------------
-- ## Things
local thing, col = {ako="thing"}, {ako="col"}
local lib, num,sym = {ako="lib"}, {ako="num"}, {ako="sym"}
local skip = {ako="skip"}
local row, tbl = {ako="row"}, {ako="tbl"}

-- All `thing`s have a unique id.
do local id=0
   function thing.new() id=id+1; return {Is=thing,Id=id} end end

---------------------
-- ## Shortcuts
local function add(i, ...) return i.Is.add(i, ...) end
local function cell(x) return not(type(x)=="string" and x=="?") end

---------------------
-- ## Column summaries
function col.factory(j,s,t) 
  local tmp = sym
  if s:find(of.ch.num) then tmp = num  end
  if s:find(of.ch.less) then tmp = num  end
  if s:find(of.ch.more) then tmp = num  end
  if s:find(of.ch.sym) then tmp = sym  end
  if s:find(of.ch.skip) then tmp = skip end
  t.cols[j] = tmp.new(j,s)
  if s:find(of.ch.klass) then t.class = t.cols[j] end 
  end

-- Generic columns
function col.new(pos,txt,    i) 
  i     = thing.new()
  i.Is  = col
  i.pos = pos or 0
  i.txt = txt or ""
  i.w   = i.txt:find(of.ch.less) and -1 or 1
  return i end

-- Numeric columns
function num.new(pos,txt,    i)  -- contractor
  i = col.new(pos,txt)
  i.Is, i.mu, i.m2, i.sd, i.n = num, 0, 0, 0, 0
  i.lo = math.maxinteger
  i.hi = math.mininteger
  return i end

function num.add(i,x) -- update
  if cell(x) then
    i.n = i.n + 1
    local d = x - i.mu
    i.mu = i.mu + d / i.n
    i.m2 = i.m2 + d*(x - i.mu)
    i.sd = i.m2<0 and 0 or (i.n<2 and 0 or (i.m2/(i.n -1))^0.5)
    i.lo = math.min(i.lo,x)
    i.hi = math.max(i.hi,x) end
  return x end

-- No-op columns
function skip.new(pos,txt,    i) -- constructor
  i = col.new(pos,txt)
  i.Is = skip 
  return i end

function skip.add(i, x) return x end -- update

-- Symbolic columns
function sym.new(pos,txt,    i)  -- constructor
  i = col.new(pos,txt)
  i.Is, i.seen, i.most = sym, {}, 0
  return i end

function sym.add(i, x) -- update
  if cell(x) then
    i.seen[x] = (i.seen[x] or 0) + 1
    if i.seen[x] > i.most then i.most,i.mode = i.seen[x],x end end
  return x end 

---------------------
-- ## Row
function row.new(tbl,cells,     i) -- constructor
  i = thing.new()
  i.Is, i.cells, i.bins = num, {}, {}
  for _,col in pairs(tbl.cols) do 
    i.cells[col.pos] = add(col, cells[col.pos])
    i.bins[col.pos] = i.cells[col.pos] end 
  return i end

-- ## Tables 
-- Hold rows
function tbl.new(i)  -- constructor
  i = thing.new()
  i.Is, i.cols, i.rows = tbl, {}, {}
  return i end

function tbl.add(i, t)  -- update
  if #i.cols==0 then 
    for j,x in pairs(t) do col.factory(j,x,i) end 
  else
    i.rows[(#i.rows)+1] = row.new(i,t) end end

-- Read from files
function tbl.read(i,f) 
  for t in lib.csv(f) do tbl.add(i, t) end
  return i end

---------------------
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
  print(s..'}') end

-- Print nested tables. 
-- Don't show private slots (those that start with `_`);
-- show slots in sorted order;
-- if `pre` is specified, then  print that as a prefix.
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
      tmp = tmp:gsub("[\t\r ]*","") -- no whitespace
               :gsub("#.*","") -- no comemnts
      t   = lib.split(tmp) 
      tmp = io.read()
      if #t > 0 then 
        for j,x in pairs(t) do t[j] = tonumber(x) or x end
        return t end
    else
      io.close(stream) end end end

-- Split the string `s` on separator `c`, defaults to "." 
function lib.split(s,     c,t)
  t, c = {}, c or ","
  for y in string.gmatch(s, "([^" ..c.. "]+)") do t[#t+1] = y end
  return t end

-- ## Return
return {of=of, lib=lib, num=num, sym=sym, tbl=tbl}
