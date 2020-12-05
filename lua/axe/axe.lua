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

local Of  = {
  synopois= "axe: optimization = cluster + contrast",
  author  = "Tim Menzies, timm@ieee.org",
  license = "MIT",
  year    = 2020,
  seed    = 1,
  ch      = {skip="?", klass="!",sym="_", 
             num=":", more=">", less="<"},
  row     = {p=2,cols="ys"},
  keys    = {c=20, n=100, x=20, z=2}}

--- ## Things

local Lib  = {} 
local Col  = {}
local Num  = {ako="Num", pos=0,txt="",n=0, 
              mu=0, m2=0, sd=0, lo=math.huge, hi= -math.huge}
local Sym  = {ako="Sym", pos=0,txt="",n=0, 
              seen={}, most=0,mode=true}
local Skip = {ako="Skip"}
local Row  = {ako="Row",  cells={}, bins={}}
local Tbl  = {ako="Tbl",  rows={}, cols={},ys={},xs={}, dist={}}

---------------------
-- ## Shortcuts
local isa
local function cell(x) return not(type(x)=="string" and x=="?") end

---------------------
-- ## Column summaries
<<<<<<< HEAD
function col.factory(j,s,t) 
  local tmp = sym
  if s:find(of.ch.num) then tmp = num  end
  if s:find(of.ch.less) then tmp = num  end
  if s:find(of.ch.more) then tmp = num  end
  if s:find(of.ch.sym) then tmp = sym  end
  if s:find(of.ch.skip) then tmp = skip end
  t.cols[j] = tmp.new(j,s)
  if s:find(of.ch.klass) then t.class = t.cols[j] end end

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
  i.lo = math.huge
  i.hi = -math.huge
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
=======
function Col.factory(j,s,t) 
  local tmp,aka = Sym, t.xs
  if s:find(Of.ch.num)  then tmp     = Num        end
  if s:find(Of.ch.less) then tmp,aka = Num, t.ys  end
  if s:find(Of.ch.more) then tmp,aka = Num, t.ys  end
  if s:find(Of.ch.sym)  then tmp     = Sym        end
  if s:find(Of.ch.skip) then tmp,aka = Skip,{}    end
  local x = tmp.new(j,s)
  if s:find(Of.ch.klass) then t.class,aka = x,t.ys end 
  t.cols[j] = x
  aka[j]= x end

function Skip.new(n,s) return isa(Skip,{txt=s, pos=n}) end 
function Sym.new(n,s)  return isa(Sym, {txt=s, pos=n}) end
function Num.new(n,s) 
  local  x=isa(Num, {txt=s, pos=n})
  x.w = x.txt:find(Of.ch.less) and -1 or 1
>>>>>>> 398030805e4818549d9c104e66ef2bd726e2295b
  return x end

function Skip:add(x) return x end 

function Sym:add(x) 
  if cell(x) then
    self.seen[x] = (self.seen[x] or 0) + 1
    if self.seen[x] > self.most then 
      self.most, self.mode = self.seen[x], x end end
  return x end 

function Sym:dist(x,y) return x==y and 0 or 1 end

function Num:add(x) 
  if cell(x) then
    self.n = self.n + 1
    local d = x - self.mu
    self.mu = self.mu + d / self.n
    self.m2 = self.m2 + d*(x - self.mu)
    self.sd = self.m2<0 and 0 or (
              self.n<2  and 0 or (
              (self.m2/(self.n -1))^0.5))
    self.lo = math.min(self.lo,x)
    self.hi = math.max(self.hi,x) end
  return x end

function Num:norm(x) return (x - self.lo) / (self.hi - self.lo + 1E-32) end
function Num:dist(x,y)
  if      not cell(x) then y   = self:norm(y); x=y>0.5 and 0 or 1 
  else if not cell(y) then y   = self:norm(x); y=x>0.5 and 0 or 1 
  else                     x,y = self:norm(x), self:norm(y) end end
  return math.abs(x-y) end 

---------------------
-- ## Row
function Row.new(row,tbl)
  local i = isa(Row)
  for _,col in pairs(tbl.cols) do 
    i.cells[col.pos] = col:add(row[col.pos])
    i.bins[col.pos] = i.cells[col.pos] end 
  return i end

function Row:dist(other,tbl, cols,      x,y,d1)
  local d,n,p = 0,1E-32,Of.row.p
  for _,col in pairs(tbl[cols or Of.row.cols]) do
    x = self.cells[col.pos]
    y = other.cells[col.pos]
    d1= (not cell(x) and not cell(y)) and 1 or col:dist(x,y)
    d = d+d1^p 
    n = n+1 end
  return (d/n)^(1/p) end

-- ## Tables 
function Tbl.new() return isa(Tbl) end

function Tbl:add(t)  
  if #self.cols==0 then 
    for j,x in pairs(t) do Col.factory(j,x,self) end 
  else
    self.rows[(#self.rows)+1] = Row.new(t,self) end end

-- Read from files
function Tbl.read(f,    t) 
  t=Tbl.new()
  for row in Lib.csv(f) do t:add(row) end
  return t end

---------------------
-- ## Cluster

local Cluster={enough=16,lvl=0,leaves={},hi=nil,lo=nil, upper={},lower={},c=0}
function Cluster.new(t) return isa(Cluster,{tbl=t}) end


---------------------
-- ## Lib

function Lib.copy(obj,   old,new)
  if type(obj) ~= 'table' then return obj end
  if old and old[obj] then return old[obj] end
  old, new = old or {}, {}
  old[obj] = new
  for k, v in pairs(obj) do new[k]=Lib.copy(v, old) end
  return new end

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

isa = Lib.isa

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

-- Warn about locals that have escaped into the global space
function Lib.rogues(    ignore,match)
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
function Lib.csv(file,     stream,tmp,t)
  stream = file and io.input(file) or io.input()
  tmp    = io.read()
  return function()
    if tmp then
      tmp = tmp:gsub("[\t\r ]*","") -- no whitespace
               :gsub("#.*","") -- no comemnts
      t   = Lib.split(tmp) 
      tmp = io.read()
      if #t > 0 then 
        for j,x in pairs(t) do t[j] = tonumber(x) or x end
        return t end
    else
      io.close(stream) end end end


-- Split the string `s` on separator `c`, defaults to "." 
function Lib.split(s,     c,t)
  t, c = {}, c or ","
  for y in string.gmatch(s, "([^" ..c.. "]+)") do t[#t+1] = y end
  return t end

-- return any item in a list
function Lib.any(a) return a[1 + math.floor(#a*math.random())] end

-- ## Return
return {Of=Of, Lib=Lib, Num=Num, Sym=Sym, Tbl=Tbl}
