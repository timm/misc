-- vim: set ts=2 sw=2 sts=2 et: 

rulr.lua: an experiment in incremental rule learning.      
@2024, Tim Menzies, <timm@ieee.org>, BSD-2 license.

```lua
local NUM  = {} -- information on numeric columns
local SYM  = {} -- information on symbolic columns
local DATA = {} -- place to store all the columns 
local COLS = {} -- a factory that makes NUMs and SYMs
local RANGE= {} -- place to store an upper and lower bound
local l    = {} -- place to store misc functions, defined later
local b4   = {} -- used by rogue() to find typos in var names
for k,_ in pairs(_ENV) do b4[k]=k end 

local function new(class, object)  -- how we create instances
  class.__index=class; setmetatable(object, class); return object end
```

This program is an experiment in incremental learning via the
Chebyshev (pronounced cheh-bee-shev) maximum metric.  The Chebyshev
distance _c_ returns the maximum difference between two points over
any of their axis values.

```lua
local function chebyshev(row,ycols,      c,tmp)
  c = 0
  for _,col in pairs(ycols) do
    tmp = col.norm(row[col.at]) -- normalize  0..1 
    c = math.max(d, math.abs(col.best - tmp))
  return 1 - c end -- so LARGER values are better
```

We want something to maximize so we will use _C=1-c_ (so _larger_
values  of _d_ are _better_).  When reading tabular data, we assume
the data has columns that are either independent `x` values or
dependent `y` goals.  If the `x` values are  discretized into ranges.
those ranges can be scored as the sum of the _C_ s seen for that
range.  Then, when we build rules, we favor the ranges with the
largest _d_ values.

To keep things simple, we will discretize numerics into seven ranges.
This value of seven is a magic configuration parameter set via
"engineering judgment" (a.k.a.  guessing).  The variable "the"
stores that magic number,  along with any other configuration
options.

```lua
local the = {ranges = 7,
             big    = 1E30.
             seed  = 1234567891,
             train = "auto93.csv"}
```

This code will need classes to handle SYMbolic and NUMeric columns.
NUMs summarize a stream of numbers.
NUMs know their column   `name`, the column `pos`ition, the `lo`
and `hi` value, as well their column mean `mu` and standard deviation
`sd`.  

```lua
function NUM.new(name,pos)
  return new(NUM, {name=name, pos=pos, n=0, mu=0, m2=0, sd=0, 
                   lo=1E30, hi= -1E30, ranges={},
                   goal = (s or ""):find"-$" and 0 or 1}) end
```

When adding a new value to  a NUM, we use the Welford 
algorithm [^welford] to incrementally update the means and standard deviations.

[^welford]: https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance

```lua
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
```

Now that `mu` and `sd` are updated incrementally, that means that
for each row, we can map any number into some integer index
`1..the.ranges`.  To do that, we report what area accumulates under
a Gaussian curve below that number. This will be be some
value 0..1 which, if we multiple by `the.ranges`, this return the
relevant range
index.

```lua
local function _range(col,x,     r)
  r = col:range(x)
  col.ranges[r] = col.ranges[r] or RANGES.new(col,lo)
  return col.ranges[r] end

function NUM:range(x,     tmp)
  tmp = self:cdf(x) * the.range // 1 + 1 -- map to 0.. the.range+1
  return  max(1, min(the.ranges, tmp)) end -- keep in bounds

function NUM:areaBelow(x,      z,fun)
  fun = function(z) return 1 - 0.5*2.718^(-0.717*z - 0.416*z*z) end
  z = (x - self.mu) / self.sigma
  return z >= 0 and fun(z) or 1 - fun(-z) end
```

(Aside: `NUM:areaBelow()` uses the Min (1989) 
approximation to the cumulative distribution function [^min].)

[^min]: As described in <em>Approximations to Standard Normal Distribution
Function</em>, Ramu Yerukala and Naveen Kumar Boiroju, International
Journal of Scientific & Engineering Research, Volume 6, Issue 4,
April-2015 515 ISSN 2229-5518
https://www.ijser.org/researchpaper/Approximations-to-Standard-Normal-Distribution-Function.pdf
While there are better approximations than Lin (1989), they are
more elaborate. Lin (1988) is a good balance between simplicity
and low error rates.

```lua
function SYM.new(name,pos)
  return new(SYM, {name=name, pos=n, n=0, has={}, mode=nil, most=0}) end 

function SYM:add(x)
  if x ~= "?" then
    self.n = 1 + self.n
    self.has[x] = 1 + (self.has[x] or 0)
    if self.has[x] > self.most then 
      self.most, self.mode = self.has[x], x end end end

function SYM:range(x) return end

```

## things

Then we need

Also, we u=must use

```lua
function COLS.new(names,     self,col)
  self = l.is(COLS, { all={}, x={}, y={}, names=names })
  for n,s in pairs(names) do self:add2Col(n,s) end
return self end

function COLS:add2Col(n,s,    col)
  col= (s:find"^[A-Z]" and NUM or SYM).new(s,n)
  l.push(self.all, col)
  if not s:find"X$" then 
    l.push(s:find"[-+!]$" and self.y or self.x, col) end end

function COLS:add(t)
  for _,cols in pairs{self.x, self.y} do
    for _,col in pairs(cols) do 
      col:add(t[col.pos]) end end 
  return t end
```

Now I have thigns tos about johns mustache

```lua
function DATA.new(it,   self) 
  self = l.is(DATA, {rows={}, cols=COLS.new(it())}) 
  for t in it do self:add(t) end  
  return self end

function DATA:add(t)
  l.push(self.rows, self.cols:add(t)) end

 -------------------------------------------------------
--------------------------------------------------------
   
--------------------------------------------------------
function SYM:mid() return self.mode end
function NUM:mid() return self.mu end

function SYM:div() return l.entropy(self.has) end
function NUM:div() return self.sd end

function DATA:mids(cols) 
  return l.map(cols or self.cols.y, function(col) return l.rnd(col:mid()) end) end

--------------------------------------------------------
local RANGE={}
function RANGE.new(pos,name,lo,hi,n)
  return l.is(RANGE,{pos=pos, name=name, lo=lo, hi=hi or lo, n=n or 0, ys={}}) end

function RANGE:add(x,y)
  self.n  = self.n + 1
  self.lo = math.min(x, self.lo)
  self.hi = math.max(x, self.hi)
  self.ys[y] = 1 + (self.ys[y] or 0) end

function RANGE:mergeable(other, small)
  both      = RANGE.new(self.pos, self.name, self.lo, other.hi, self.n + other.n)
  both.ys   = l.sumDict(self.y, other.ys)
  e1,e2,e12 = l.entropy(self.ys), l.entropy(other.ys), l.entropy(both.ys)
  if self.n < small or other.n < small or e12 <= (self.n*e1 + other.n*e2) / both.n then
    return both end end 

function DATA:bins(col,klasses)
  bins,n = {},0
  for klass,rows in pairs(klasses) do
    for _,row in pairs(rows) do
      n += 1
      x = row[col.pos]
      if x != "?" then 
        k = col:bins(x) 
        bins[k] = bins[k] or RANGE.new(col.pos, col.name, x)
        bins[k]:add(x,klass) end end end 
  return col:merges(sort(bins, by"lo"),  n/the.bins) end

function SYM:bin(x) return x end
function NUM:bin(x) return math.min(the.bins, 1+((the.bins * self:norm(x))//1)) end
 
function SYM:merges(x,_) return x end
function NUM:merges(b4,enough,    j,now,a,tmp)
  j, now = 1, {}
  while j <=  len(b4) do
    a = b4[j]
    if j <  len(b4) then
      tmp = a:mergable(b4[j+1],enough) 
      if tmp then
        a = tmp
        j = j+1 end end
    l.push(now,a)
    j = j+1 
  end
  if len(now) < len(b4) then return self:merges(now,enough) end
  for j=2,len(now) do
    now[j].lo    = now[j-1].hi
    now[1].lo    = -1E30
    now[#now].hi =  1E30 end
  return now end

--------------------------------------------------------
-- ## Misc Functions

-- Shortcuts
l.cat = table.concat
l.fmt = string.format

-- Objects
    -- ### Lists
function l.by(s) return function(a,b) return a[s] < b[s] end end

-- returns a copy of `t`, sorted.
function l.sort(t,fun,     u) 
  u={}; for _,v in pairs(t) do u[1+#u]=v end; table.sort(u,fun); return u end

function l.push(t,x) t[1+#t]=x; return x end

function l.items(t,     k,n)
  k,n=0,#t
  return function() if k < n then k=k+1; return t[k] end end end

function l.map(t,f,     u) 
  u={};  for k,v in pairs(t) do u[1+#u] = f(v) end; return u end

function l.kap(t,f,     u) 
  u={};  for k,v in pairs(t) do u[1+#u] = f(k,v) end; return u end

function l.sumDicts(d1,d2,     d12)
  d12={}
  for _,d in pairs{d1, d2} do
    for k,v in pairs(d) do
      d12[k] = v + (d12[k] or 0) end end
  return d12 end

-- ### Maths
function l.entropy(t,   e,N)
  N=0; for _,n in pairs(t) do N = N + n end
  e=0; for _,n in pairs(t) do e = e - (n/N)*math.log(n/N,2) end 
  return e end

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

## Set-up actions

math.randomseed(the.seed)
return {the=the, lib=l,DATA=DATA,SYM=SYM,NUM=NUM,COLS=COLS}
```
