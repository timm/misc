-- # RULR 
-- 
-- <img align=right src=marsQueen.png width=250>
-- 
-- rulr.lua: an experiment in incremental rule learning.      
-- @2024, Tim Menzies, <timm@ieee.org>, BSD-2 license.
-- 
-- This code is an experiment in incremental rule learning via the
-- Chebyshev (pronounced cheh-bee-shev) maximum metric.  Incremental
-- learning is important since, often, there is so much to explore
-- that we cannot look at it all.  So how much do we lose by jumping
-- in early and generating a model before all the facts are in?
-- Optimistically, we hope for an "early plateau" effect where, after
-- some point, we stop learning new things. This code will test that
-- optimism.
-- 
-- - [RULR](#rulr)
--   - [Conventions](#conventions)
--     - [Data Conventions](#data-conventions)
--     - [Coding Conventions](#coding-conventions)
--     - [Name space](#name-space)
--   - [Inside the Code](#inside-the-code)
--     - [The Chebyshev Function](#the-chebyshev-function)
--     - [Class RANGEs](#class-ranges)
--     - [Config (stored in "the")](#config-stored-in-the)
--     - [Class NUMs](#class-nums)
--     - [RANGE creation and updating](#range-creation-and-updating)
--     - [Class SYM](#class-sym)
--     - [Class COLS](#class-cols)
--     - [Class DATA](#class-data)
--   - [Misc Support code](#misc-support-code)
--   - [Start-up](#start-up)
-- 
-- 
-- ## Conventions
-- 
-- ### Data Conventions
-- 
-- Note some conventions about our data:
--   
-- - In our data, the  string "?" is used to denote a missing value.
-- - In our data,  row one  list the columns names. Those names
--   define the various roles of our columns:
--   - NUMeric column names start with an upper case letter. All
--     other columns are SYMbolic.
--   - Names ending with "+" or "-" are things we want to maximize
--     or minimize(respectively). 
--   - Anything ending in "X" is a column we should ignore.
--   - For example, here is some car data where the goals are
--     `Lbs-,Acc+,Mpg+`; i.e. we want
--     to minimize car weight and maximize our acceleration and 
--     maximize fuel consumption.
-- 
--         {Clndrs  Volume  HpX      Model  origin  Lbs-  Acc+  Mpg+}
--         -------  ------  ---      -----  ------  ----  ----  ----
--         {8       302     129      75     1       3169  12    10}
--         {8       318     150      72     1       4135  13.5  20}
--         {6       168     120      76     2       3820  16.7  20}
--         {3       70      90       73     3       2124  13.5  20}
--         {6       232     90       78     1       3210  17.2  20}
--         {6       231     110      75     1       3039  15    20}
--         {6       173     110      81     1       2725  12.6  20}
--         {4       140     92       76     1       2572  14.9  30}
--         {4       97      88       72     3       2100  16.5  30}
--         {4       90      70       76     2       1937  14.2  30}
--         {4       85      65       79     3       2020  19.2  30}
--         {4       98      65       81     1       2045  16.2  30}
--         {4       85      70       78     3       2070  18.6  40}
-- 
-- 
-- ### Coding Conventions
-- 
-- As to our coding conventions:
-- 
-- - This code is written in Lua since that is a very simple notation.
--   For a short tutorial on Lua, see "[Learn Lua in Y
--   minutes](https://learnxinyminutes.com/docs/lua/)".
-- - UPPER CASE names are classes. `XXX.new()` is the constructor for
--   class `XXX`.
-- - In function headers, anything after two spaces is an optional arg.
--   Also, anything after four spaces is a local variable. For example, looking at the
--   first two functions defined below:
--   - `c,tmp` are local variables within the `chebyshev()` function, shown below.
--   - `d` is an optional argument for `RANGE.new()` (and it is not supplied then we 
--     default `hi` to the value of `lo`).
-- - This code uses polymorphism, but no inheritance.
--    Why not use full OO? I will let others explain that.
--    See Les Hatton's comments on that 
--    [Does OO sync with how we think?](https://www.researchgate.net/publication/3247400_Does_OO_sync_with_how_we_think).
--    and see also Jack Diederich's 
--    [Stop Writing Classes](https://www.youtube.com/watch?v=o9pEzgHorH0).
-- 
-- ### Name space
-- 
-- We define the name space at top-of-file that makes it easier to
-- rearrange the code to fit the narrative.  Here is our name space:
-- 

local NUM  = {} -- info on numeric columns
local SYM  = {} -- info on symbolic columns
local DATA = {} -- place to store all the columns 
local COLS = {} -- factory to make NUMs and SYMs
local RANGE= {} -- stores ranges
local l    = {} -- stores misc functions, defined later
   
function isa(class, object)  -- how we create instances
  class.__index=class; setmetatable(object, class); return object end

local b4   = {} -- used by rogue() to find typos in var names
for k,_ in pairs(_ENV) do b4[k]=k end 
-- 
-- 
-- ## Inside the Code
-- ### The Chebyshev Function
-- 
-- We use Chebyshev since it is a very harsh critic. If any goal is not controlled
-- then Chebyshev will complain, even if every other goal is doing fine.
-- 
-- More specifically,
-- the Chebyshev distance _c_ returns the maximum difference between
-- two points over any of their axis values.
-- 

local function chebyshev(row,ycols,      c,tmp)
  c = 0
  for _,col in pairs(ycols) do
    tmp = col:norm(row[col.pos]) -- normalize  0..1 
    c = math.max(c, math.abs(col.best - tmp)) end
  return 1 - c end -- so LARGER values are better
-- 
-- We want something to maximize so we will use _d=1-c_ (so _larger_
-- values  of _d_ are _better_).
-- 
-- ### Class RANGEs
-- 
-- When reading tabular data, we assume the data has columns that are
-- either independent `x` values or dependent `y` goals.  If the `x`
-- values are  discretized into ranges, those ranges have a  `score`
-- equal to  the sum of the _d_ s seen for that range.  Then, when we
-- build rules, we favor the ranges with the largest _d_ values.
-- 

function RANGE.new(col,r)
  return isa(RANGE, {n=0, _col=col, on=r, merged={r},  _score=0}) end

function RANGE:__tostring() return l.o{name=self._col.name, merged=self.merged, n=self.n, on=self.on} end

function RANGE:add(x,d)
  self.n = self.n + 1
  self._score = self._score + d  end

function RANGE:score(      s)
  s= self._score/self.n; return s < 0 and 0 or s end
-- 
-- Two adjacent  RANGEs can be merged if either contains too few examples or if
-- their scores are very similar. The merged range has a score that the weighted sum
-- of the two parts.
-- 

function RANGE.merge(i,j,small,dull,     k)
  out = i + j
  if i.n < small or j.n < small then return out end 
  if i:score() < dull and j:score() < dull then return out end
  if math.abs(i:score() - j:score()) < dull then return out end end

function RANGE:__add(other)
  out   = RANGE.new(self.col,self.r)
  out.n = self.n + other.n
  out.on = self.on
  out._score = (self.n * self._score + other.n * other._score)/out.n
  for _,r in pairs{self.has, other.has} do l.push(out.has, r) end
  return out end
-- 
-- 
-- We say a RANGE `selects()` a row if the row's value for that column
-- falls within that range.
-- 

function RANGE:selects(row) 
  x = row[self.col.pos] -- if value if "dont know", then assume true
  if x=="?" then return true end
  wants = self.col:range(x)
  for _,has in pairs(self.has) do if wants==has then return true end end end
-- 
-- Just as an aside, once we coded up RANGEs
-- for NUMeric ranges, it was fun to see that exactly the same code
-- worked for SYMbolic ranges.
-- 
-- ### Config (stored in "the")
-- 
-- To keep things simple, we will discretize NUME+erics into seven
-- ranges.  This value of seven is a magic configuration parameter set
-- via "engineering judgment" (a.k.a.  guessing).  The variable "the"
-- stores that magic number,  along with any other configuration
-- options.
-- 

local the = {ranges = 7,
             big    = 1E30,
             dull   = 0.05,
             seed  = 1234567891,
             train = "auto93.csv"}
-- 
-- 
-- ### Class NUMs 
-- 
-- This code will need classes to handle SYMbolic and NUMeric columns.
-- Both kinds of columns support comparison and sorting but only
-- NUMerics support mathematical operations (sich as add or subtract).
-- 
-- NUMs summarize a stream of numbers.  NUMs know their column   `name`,
-- the column `pos`ition, the `lo` and `hi` value, as well their column
-- mean `mu` and standard deviation `sd`.
-- 

function NUM.new(name,pos)
  return isa(NUM, {name=name, pos=pos, n=0, ranges={}, 
                   mu=0, m2=0, sd=0, lo=1E30, hi= -1E30,  
                   best = (name or ""):find"-$" and 0 or 1}) end

-- 
-- Note that NUM has a `best` slot which holds the best value possible
-- for this column. If a column name ends in "-" then we say that we
-- seek to minimize this column (in which case, the `best` is 0).  For
-- this to work, we must first normalize the goals to the range 0..1
-- (which is handled via `norm()`):
-- 

function NUM:norm(x)
  return x=="?" and x or (x - self.lo) / (self.hi - self.lo + 1/the.big) end
-- 
-- 
-- When adding a new value to  a NUM, we use the Welford algorithm
-- [^welford] to incrementally update the means and standard deviations.
-- 
-- [^welford]: https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance
-- 

function NUM:add(x,     d) 
  if x ~= "?" then
    self.n  = 1 + self.n
    self.lo = math.min(x, self.lo)
    self.hi = math.max(x, self.hi)
    d       = x - self.mu
    self.mu = self.mu + d/self.n
    self.m2 = self.m2 + d*(x - self.mu)
    self.sd = (self.m2/(self.n - 1 + 1/the.big))^0.5 end 
  return x end

-- 
-- ### RANGE creation and updating
-- 
-- Now that `mu` and `sd` are updated incrementally, that means that
-- for each row, we can map any number into some integer index
-- `1..the.ranges`.  To do that, we report what area accumulates under
-- a Gaussian curve below that number. This will be be some value 0..1
-- which, if we multiple by `the.ranges`, this return the relevant
-- range.
-- 

function NUM:range(x,     tmp)
  tmp = self:area(x) * the.ranges // 1 + 1 -- maps x to 0.. the.range+1
  return  math.max(1, math.min(the.ranges, tmp)) end -- keep in bounds

function NUM:area(x,      z,fun)
  fun = function(z) return 1 - 0.5*2.718^(-0.717*z - 0.416*z*z) end 
  z = (x - self.mu) / self.sd
  return z >= 0 and fun(z) or 1 - fun(-z) end
-- 
-- 
-- (Aside: `NUM:area()` uses the Lin (1989) approximation to the
-- cumulative distribution function [^min].)
-- 
-- [^min]: As described in [Approximations to Standard Normal Distribution
-- Function](https://www.ijser.org/researchpaper/Approximations-to-Standard-Normal-Distribution-Function.pdf)
-- by Ramu Yerukala and Naveen Kumar Boiroju, International Journal
-- of Scientific & Engineering Research, Volume 6, Issue 4, April-2015
-- 515 ISSN 2229-5518 While there are better approximations than Lin
-- (1989), they are more elaborate. Lin (1988) is a good balance between
-- simplicity and low error rates.
-- 
-- ### Class SYM
-- 
-- Turning now to SYMbolic columns, these have nearly all the same
-- slots as NUMbers. But also, SYMs  keep  a count of the symbols
-- `seen` so far as well as the most common symbol (which is called
-- the `mode`).
-- 

function SYM.new(name,pos)
  return isa(SYM, {name=name, pos=pos, n=0, ranges={},
                   seen={}, mode=nil, most=0}) end 

function SYM:add(x)
  if x ~= "?" then
    self.n = 1 + self.n
    self.seen[x] = 1 + (self.seen[x] or 0)
    if self.seen[x] > self.most then 
      self.most, self.mode = self.seen[x], x end end end
-- 
-- 
-- The `arrange()` function (shown above) needs to know how to convert
-- a value into a range.  Each SYMbolic value is its own range:
-- 

function SYM:range(x) return x end
-- 
-- 
-- ### Class COLS
-- 
-- Recalling the daa example shown above, our data files have an first
-- row that names our columns:
-- 
--       Clndrs, Volume, HpX, Model, origin, Lbs-,Acc+, Mpg+
-- 
-- The COLS class is a factory that can take  that list of names and
-- creates a NUMeric class (for names starting with upper case), goals
-- (for anything ending in "+" or "-").  It also knows to skip over
-- names ending with "X" (e.g. "HpX").
-- 

function COLS.new(names,     self,col)
  self = isa(COLS, { all={}, x={}, y={}, names=names })
  for n,s in pairs(names) do self:newColumn(n,s) end
return self end
-- 
-- All our NUMs and SYMs get stored in `self.all`. And, for ease of
-- processing, some are also stores in `self.x` and `self.y` (for the
-- independent and dependent variables)
-- 

function COLS:newColumn(n,s,    col)
  col = (s:find"^[A-Z]" and NUM or SYM).new(s,n) 
  l.push(self.all,col)
  if not s:find"X$" then 
    l.push(s:find"[-+!]$" and self.y or self.x, col) end end 
-- 
-- When COLS get updated with a `row`, they find the Chebyshev distance
-- `d` (calculated above) for that `row`. This is used to update the
-- column information, as well as the RANGEs of each column (and it no `d`
-- is supplied, we skip updating the ranges)
-- 

function COLS:add(row,  d,      x)
  for _,cols in pairs{self.x, self.y} do
    for _,col in pairs(cols) do 
       x = row[col.pos]
       if x ~= "?" then
          col:add(row[col.pos])
          if d then self:arrange(col,x,d) end end end end end
          
function COLS:arrange(col,x,  d,     r)
  r = col:range(x)
  col.ranges[r] = col.ranges[r] or RANGE.new(col,r,x)
  col.ranges[r]:add(x,d)  end 
-- 
-- 
-- ### Class DATA
-- 
-- The DATA class ties everything together. When it reads the first
-- `row` of the data, it calls `COLS.isa()` to create the columns.
-- When it reads the other `row`s, it updates those columns with in
-- information from each `row`.
-- 

function DATA.new(src,   self) 
  for row in src do
    if self then  -- this is some row after the first row
      self:add(row)   
    else  -- this is the first row
      self = isa(DATA, {rows={}, cols=COLS.new(row)}) end end
  return self end

function DATA:add(row)
  l.push(self.rows, row)
  self.cols:add(row, chebyshev(row, self.cols.y)) end
-- 
-- 

function DATA:ranges(     fun,out)
  out = {}
  fun = function(r) return r:score() end 
  for _,col in pairs(self.cols.x) do 
    for _,r in pairs(DATA:merge(col.ranges, #(self.rows)/the.ranges, the.dull)) do
       l.push(out,r) end end 
  return out end 

function DATA:merge(b4,small,dull,  isSorted,      a,ab,now,i)
  b4 = isSorted and b4 or l.sort(b4, l.by"on")  
  i, now = 1, {}
  while i <= #b4 do
     a = b4[i] 
     if i < #b4 then
       ab = a:merge(b4[i+1],small,dull)
       if ab then
         a = ab
         i = i + 1 end end
    l.push(now,a)
    i = i + 1 end
  return #now == #b4 and b4 or self:merge(now,small,dull,true) end
-- 
-- 

function SYM:mid() return self.mode end
function NUM:mid() return self.mu end

function SYM:div() return l.entropy(self.has) end
function NUM:div() return self.sd end

function DATA:mids(cols) 
  return l.map(cols or self.cols.y, function(col) return l.rnd(col:mid()) end) end
-- 
-- 
-- ## Misc Support code
-- 
-- Standard one-liners:

l.cat = table.concat
l.fmt = string.format
-- 
-- List stuff

function l.sort(t,fun,     u) -- return a copy of `t`, sorted using `fun`,
  u={}; for _,v in pairs(t) do u[1+#u]=v end; table.sort(u,fun); return u end

function l.by(x) return function(a,b) return a[x] < b[x] end end

function l.on(fun) return function(a,b) return fun(a) < fun(b) end end

function l.push(t,x) t[1+#t]=x; return x end
 
function l.map(t,f,     u) u={}; for k,v in pairs(t) do u[1+#u]= f(v)   end; return u end
function l.kap(t,f,     u) u={}; for k,v in pairs(t) do u[1+#u]= f(k,v) end; return u end
-- 
-- Stuff for reading csv files.

function l.csv(src)
  src = src=="-" and io.stdin or io.input(src)
  return function(      s)
    s = io.read()
    if s then return l.cells(s) else io.close(src) end end end

function l.cells(s,    t)
  t={}; for s1 in s:gsub("%s+", ""):gmatch("([^,]+)") do t[1+#t]=l.coerce(s1) end
  return t end

function l.coerce(s,     _other) 
  _other = function(s) if s=="nil" then return nil  end
                       return s=="true" or s ~="false" and s or false end 
  return math.tointeger(s) or tonumber(s) or _other(s:match'^%s*(.*%S)') end
-- 
-- Pretty print stuff.

function l.oo(t) print(l.o(t)); return t end

function l.o(t,    _list,_dict,u)
  if type(t) == "number" then return tostring(l.rnd(t)) end
  if type(t) ~= "table" then return tostring(t) end
  _list = function(_,v) return l.o(v) end 
  _dict = function(k,v) if not tostring(k):find"^_" then return l.fmt(":%s %s",k,l.o(v)) end end
  u = l.kap(t, #t==0 and _dict or _list)
  return "{" .. l.cat(#t==0 and l.sort(u) or u ," ") .. "}" end 

function l.rnd(n, ndecs)
  if type(n) ~= "number" then return n end
  if math.floor(n) == n  then return math.floor(n) end
  local mult = 10^(ndecs or 2)
  return math.floor(n * mult + 0.5) / mult end
-- 
-- Linting for `rogue` variables (e.g. misspelt, not declared as local).

function l.rogues() 
  for k,v in pairs(_ENV) do if not b4[k] then print("Rogue?",k,type(v)) end end end
-- 
-- 
-- Object stuff


-- 
-- 
-- ## Start-up
-- 

math.randomseed(the.seed)
return {the=the, lib=l,DATA=DATA,SYM=SYM,NUM=NUM,COLS=COLS}
-- 
