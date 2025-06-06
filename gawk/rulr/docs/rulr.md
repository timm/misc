

rulr.lua: an experiment in icnremental rule learning.      
@2024, Tim Menzies, <timm@ieee.org>, BSD-2 license.


This program is an experiment in incremental learning via the
Chebyshev maximum metric.  This code reads a table of rows, where
the columns are either indepedent `x` values or dependent `y` goals.
As new rows arrive, some are _best_; i.e. their y values are closest
to the desired best value for that column.


```lua
local function chebyshev(row,ycols,      d,tmp)
  d = 0
  for _,col in pairs(ycols) do
    tmp = col.norm(row[col.at]) -- normalize  0..1 
    d = math.max(d, math.abs(col.best - tmp))
  return d end
```



If a row is not _best_, then we will call it _rest_.  This code
sorts  ranges according to how often they are seen in the _best_
and _rest_ rows. We will build rules, prefering ranges that frequently
occur in _best_ (as well as being more probable in _best_ than
_rest_).


```lua
local function prefer(range,bests,rests,nBests, nRests)
  local b,r,support,probability
  b = bests[range] / nBests
  r = rests[range] / nRests
  support = b
  probability = b/(b+r)
  return support * probability end
```



If $\mu,\sigma$ are the standard deviation of the distance seen to
date, then  a _best_ row has Chebyshev distance $d$ is $(d-\mu)/\sigma < T$.
Parameters like $T$ are set via engineering judgement (i.e.  guesing).
$T$ is stored in a special variable "the", along with all our other
configuration options.  FYI, values like $T=-1.28$ or $T=-1$ select
for the top top one-tenth or one-sixth of all rows.


```lua
local the = {T     = -1.28,
             seed  = 1234567891,
             train = "auto93.csv"}
```



This code will need some classes:


```lua
local NUM = {} -- information on numeric columns
local SYM = {} -- information on symbolic columns
local DATA= {} -- place to store all the columns 
local COLS= {} -- a factory that makes NUMs and SYMs
local RANGE= {} -- place to store an upper and lower bound

Also, we need some trivia to support our Lua code:
```



```lua
local l = {}; -- place to store misc functions
local b4 = {}; for k,_ in pairs(_ENV) do b4[k]=k end -- used by rogue()
local function new(class, object)  -- how we create instances
  klass.__index=class; setmetatable(object, class); return object end
```



Our NUMs know their column   `name`, column `pos`ition, the `lo`
and `hi` value, as well their column mean `mu` and standar deviation
`sd`.  


```lua
 function NUM.new(name,pos)
  return new(NUM, {name=name, pos=pos, n=0, mu=0, m2=0, sd=0, 
                   lo=1E30, hi= -1E30, 
                   goal = (s or ""):find"-$" and 0 or 1}) end
```



When adding a new value to  a NUM, we use the Welford incremetal
algorithm [^welford] to update the means and standard deviations.


[^welford]: https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Welford's_online_algorithm


```lua
function NUM:add(x,     d) 
  if x ~= "?" then
    self.n  = 1 + self.n
    self.lo = math.min(x, self.lo)
    self.hi = math.max(x, self.hi)
    d       = x - self.mu
    self.mu = self.mu + d/self.n
    self.m2 = self.m2 + d*(x - self.mu)
    self.sd = (self.m2/(self.n - 1 + 1/1E30))^0.5 end end
   
For that to work, when we define numeric columns, recognize which of those
are goals that we want to either minimize or maximze.
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

Now I have thigns tos about johns mustache
```



```lua
function DATA.new(it,   self) 
  self = l.is(DATA, {rows={}, cols=COLS.new(it())}) 
  for t in it do self:add(t) end  
  return self end

function DATA:add(t)
  l.push(self.rows, self.cols:add(t)) end

 -------------------------------------------------------
function SYM.new(name,pos)
  return l.is(SYM, {name=name, pos=n, n=0, has={}, mode=nil, most=0, 
                    heaven=(s or ""):find"-$" and 0 or 1}) end

function SYM:add(x)
  if x ~= "?" then
    self.n = 1 + self.n
    self.has[x] = 1 + (self.has[x] or 0)
    if self.has[x] > self.most then self.most, self.mode = self.has[x], x end end end

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

function l.cdf(x,mu,sigma,     z,fun)
  fun = function(z) return 1 - 0.5*2.718^(-0.717*z - 0.416*z*z) end
  z = (x - mu) / sigma
  return z >= 0 and fun(z) or 1 - fun(-z) end

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
```



## Set-up actions


```lua
math.randomseed(the.seed)
return {the=the, lib=l,DATA=DATA,SYM=SYM,NUM=NUM,COLS=COLS}
```

