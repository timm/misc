-- # RULR
-- 
-- rulr.lua: an experiment in incremental rule learning.      
-- @2024, Tim Menzies, <timm@ieee.org>, BSD-2 license.
-- 
-- This program is an experiment in incremental rule learning via the
-- Chebyshev (pronounced cheh-bee-shev) maximum metric. 
-- Incremental learning is important since, often,
-- there is so much to explore that we cannot look at it all.
-- So how much do we lose by jumping in early and 
-- generating a model before all the facts are in? Optimistically,
-- we hope for an 
-- "early plateau" effect where, after some point,
-- we stop learning new things. This code will test that optimism.
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
-- As to other coding conventions:
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
-- We define the name space at top-of-file 
--   that makes it easier to
--   rearrange the code to fit the narrative. Here is our name space:
-- 

local NUM  = {} -- info on numeric columns
local SYM  = {} -- info on symbolic columns
local DATA = {} -- place to store all the columns 
local COLS = {} -- factory to make NUMs and SYMs
local RANGE= {} -- stores upper and lower bounds
local l    = {} -- stores misc functions, defined later
   
local function new(class, object)  -- how we create instances
  class.__index=class; setmetatable(object, class); return object end
   
local b4   = {} -- used by rogue() to find typos in var names
for k,_ in pairs(_ENV) do b4[k]=k end 
-- 
-- 
-- ## Inside the Code
-- ### The Chebyshev Function
-- 
-- The Chebyshev distance _c_ returns the maximum difference between
-- two points over any of their axis values.  
-- 

local function chebyshev(row,ycols,      c,tmp)
  c = 0
  for _,col in pairs(ycols) do
    tmp = col.norm(row[col.at]) -- normalize  0..1 
    c = math.max(d, math.abs(col.best - tmp)) end
  return 1 - c end -- so LARGER values are better
-- 
-- 
-- We want something to maximize so we will use _d=1-c_ (so _larger_
-- values  of _d_ are _better_).  
-- 
-- 
-- ### Class RANGEs
-- 
-- When reading tabular data, we assume
-- the data has columns that are either independent `x` values or
-- dependent `y` goals.  If the `x` values are  discretized into ranges,
-- those ranges have a  `score` equal to  the sum of the _d_ s seen
-- for that range.  Then, when we build rules, we favor the ranges
-- with the largest _d_ values. 
-- 

function RANGE.new(col,lo,  hi)
  return new(RANGE, {col=col, lo=lo, hi=hi or lo, score=0}) end

function RANGE:add(x,d)
  self.score = self.score + d 
  if x < self.lo then self.lo = x end
  if x > self.hi then self.hi = x end end
-- 
-- 
-- As In the following code, we say
-- a RANGE `selects()` a row if the row's value for that column
-- falls within that range.
-- 

function RANGE:selects(row) 
  x = row[self.col.at] -- if value if "dont know", then assume true
  return x == "?" and true or 
         self.lo <= x and x < self.hi or         -- for NUMeric ranges
         self.lo == self.hi and self.lo == x end -- for SYMbolic ranges    
-- 
-- To explain the  last line of `selects()`, once we coded up RANGEs for NUMeric ranges,
-- it was fun to see that nearly the same code worked for SYMbolic ranges,
-- with one tiny hack: SYMboic ranges have the same value for `lo` and `hi`. 
--  
-- ### Config (stored in "the")
-- 
-- To keep things simple, we will discretize NUME+erics into seven ranges.
-- This value of seven is a magic configuration parameter set via
-- "engineering judgment" (a.k.a.  guessing).  The variable "the"
-- stores that magic number,  along with any other configuration
-- options.
-- 

local the = {ranges = 7,
             big    = 1E30,
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
  return new(NUM, {name=name, pos=pos, n=0, ranges={}, 
                   mu=0, m2=0, sd=0, lo=1E30, hi= -1E30,  
                   goal = (name or ""):find"-$" and 0 or 1}) end

-- 
-- Note that NUM has a `goal` slot which holds the best value possible
-- for this column. If a column name ends in "-" then we say that we
-- seek to minimize this column (in which case, the `goal` is 0).  For
-- this to work, we must first normalize the goals to the range 0..1
-- (which is handled via `norm()`):
-- 

function NUM:norm(x)
  return x=="?" and x or (x - self.lo) / (self.hi - self.lo + 1/the.big) end
-- 
-- 
-- When adding a new value to  a NUM, we use the Welford algorithm [^welford] to incrementally update the means and standard deviations.
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
-- a Gaussian curve below that number. This will be be some
-- value 0..1 which, if we multiple by `the.ranges`, this return the
-- relevant range.
-- 

local function arrange(col,x,d,     r) -- "col" can be a NUM or a SYM
  r = col:range(x)
  col.ranges[r] = col.ranges[r] or RANGE.new(col.lo)
  col.ranges[r]:add(x,d)  end

function NUM:range(x,     tmp)
  tmp = self:cdf(x) * the.range // 1 + 1 -- map to 0.. the.range+1
  return  max(1, min(the.ranges, tmp)) end -- keep in bounds

function NUM:areaBelow(x,      z,fun)
  fun = function(z) return 1 - 0.5*2.718^(-0.717*z - 0.416*z*z) end
  z = (x - self.mu) / self.sigma
  return z >= 0 and fun(z) or 1 - fun(-z) end
-- 
-- 
-- (Aside: `NUM:areaBelow()` uses the Lin (1989) 
-- approximation to the cumulative distribution function [^min].)
-- 
-- [^min]: As described in [Approximations to Standard Normal Distribution Function](https://www.ijser.org/researchpaper/Approximations-to-Standard-Normal-Distribution-Function.pdf)
-- by Ramu Yerukala and Naveen Kumar Boiroju, International
-- Journal of Scientific & Engineering Research, Volume 6, Issue 4,
-- April-2015 515 ISSN 2229-5518
-- While there are better approximations than Lin (1989), they are
-- more elaborate. Lin (1988) is a good balance between simplicity
-- and low error rates.
-- 
-- ### Class SYM
-- 
-- Turning now to SYMbolic columns, these have nearly all the same
-- slots as NUMbers. But also, SYMs  keep  a count of the symbols
-- `seen` so far as well as the most common symbol (which is called the `mode`).
-- 

function SYM.new(name,pos)
  return new(SYM, {name=name, pos=n, n=0, ranges={},
                   seen={}, mode=nil, most=0}) end 

function SYM:add(x)
  if x ~= "?" then
    self.n = 1 + self.n
    self.seen[x] = 1 + (self.seen[x] or 0)
    if self.seen[x] > self.seen then 
      self.most, self.mode = self.seen[x], x end end end
-- 
-- 
-- The `arrange()` function (shown above) needs to know how to convert a value into a range.
-- Each SYMbolic value is its own range:
-- 

function SYM:range(x) return x end
-- 
-- 
-- ### Class COLS
-- 
-- Recalling the daa example shown above, our data files have an first row
-- that names our columns:
-- 
--       Clndrs, Volume, HpX, Model, origin, Lbs-,Acc+, Mpg+
-- 
-- The COLS class is a factory that can take  that list of names and creates a NUMeric
-- class (for names starting with upper case), goals (for anything ending in "+" or "-").
-- It also knows to skip over names edning with "X" (e.g. "HpX").
-- 

function COLS.new(names,     self,col)
  self = new(COLS, { all={}, x={}, y={}, names=names })
  for n,s in pairs(names) do l.push(self.cols, self:add2Col(n,s)) end
return self end
-- 
-- All our NUMs and SYMs get stored in `self.all`. And, for ease of processing,
-- some are also stores in `self.x` and `self.y` (for the independent and dependent variables)
-- 

function COLS:add2Col(n,s,    col)
  col = (s:find"^[A-Z]" and NUM or SYM).new(s,n) 
  if not s:find"X$" then 
    l.push(s:find"[-+!]$" and self.y or self.x, col) end end 
-- 
-- When COLS get updated with a `row`, they find the Chebyshev distance `d` 
-- (calculated above). This is used to 
--  update the column information, as well as the RANGEs of each column.
-- 

function COLS:add(row)
  for _,cols in pairs{self.x, self.y} do
    for _,col in pairs(cols) do 
       col:add(row[col.pos])
       arrange(col, row[col.pos], chebyshev(row, self.y)) end end end
-- 
-- 
-- ### Class DATA
-- 
-- The DATA class ties everything together. When it reads the first `row` of the data,
-- it calls `COLS.new()` to create the columns. When it reads the other `row`s, it updates
-- those columns with in information from that `row`.  
-- 

function DATA.new(file,   self) 
  self = new(DATA, {rows={}, cols=COLS.new(it())}) 
  for row in csv(file) do self:add(row) end  
  return self end

function DATA:add(row)
  l.push(self.rows, row)
  self.cols:add(row)  end
-- 
-- 
-- 

function SYM:mid() return self.mode end
function NUM:mid() return self.mu end

function SYM:div() return l.entropy(self.has) end
function NUM:div() return self.sd end

function DATA:mids(cols) 
  return l.map(cols or self.cols.y, function(col) return l.rnd(col:mid()) end) end
 
-- Shortcuts
l.cat = table.concat
l.fmt = string.format
 - 
-- returns a copy of `t`, sorted.
function l.sort(t,fun,     u) 
  u={}; for _,v in pairs(t) do u[1+#u]=v end; table.sort(u,fun); return u end

function l.push(t,x) t[1+#t]=x; return x end
 

function l.map(t,f,     u) 
  u={};  for k,v in pairs(t) do u[1+#u] = f(v) end; return u end

function l.kap(t,f,     u) 
  u={};  for k,v in pairs(t) do u[1+#u] = f(k,v) end; return u end
 
 

function l.rnd(n, ndecs)
  if type(n) ~= "number" then return n end
  if math.floor(n) == n  then return n end
  local mult = 10^(ndecs or 2)
  return math.floor(n * mult + 0.5) / mult end

-- ### String to Thing
function l.coerce(s,     _other) 
  _other = function(s) if s=="nil" then return nil  end
                       return s=="true" or s ~="false" and s or false end 
  return math.tointeger(s) or tonumber(s) or _other(s:match'^%s*(.*%S)') end

function l.cells(s,    t)
  t={}; for s1 in s:gsub("%s+", ""):gmatch("([^,]+)") do t[1+#t]=l.coerce(s1) end
  return t end

function l.csv(src)
  src = src=="-" and io.stdin or io.input(src)
  return function(      s)
    s = io.read()
    if s then return l.cells(s) else io.close(src) end end end

-- ### Pretty print

function l.oo(t) print(l.o(t)); return t end

function l.o(t,    _list,_dict,u)
  if type(t) ~= "table" then return tostring(t) end
  _list = function(_,v) return l.o(v) end 
  _dict = function(k,v) return l.fmt(":%s %s",k,l.o(v)) end
  u = l.kap(t, #t==0 and _dict or _list)
  return "{" .. l.cat(#t==0 and l.sort(u) or u ," ") .. "}" end 

-- ### List
function l.rogues() 
  for k,v in pairs(_ENV) do if not b4[k] then print("Rogue?",k,type(v)) end end end

-- ## Set-up actions

math.randomseed(the.seed)
return {the=the, lib=l,DATA=DATA,SYM=SYM,NUM=NUM,COLS=COLS}
-- 
-- 
-- asda
