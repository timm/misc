#!/usr/bin/env lua
-- <!-- vim: set ts=2 sw=2 sts=2 et: -->
local the={about={what="rulr",
                  why="experiment with fast clustering",
                  who="Tim Menzies",
                  when=2024,
                  copyright="BSD, two clause"},
           bins=16,
           seed=1234567891,
           train="auto93.csv"}

local l,b4={},{}; for k,_ in pairs(_ENV) do b4[k]=k end
local DATA,NUM,SYM,COLS={},{},{},{}

-------------------------------------------------------
function COLS.new(names,     self,col)
  self = l.is(COLS, { all={}, x={}, y={}, names=names })
  for n,s in pairs(names) do self:add2Col(n,s) end
  return self end

function COLS:add2Col(n,s,    col)
  col= (s:find"^[A-Z]" and NUM or SYM).new(s,n)
  l.push(self.all, col)
  if not s:find"X$" then 
    l.push(s:find"[-+!]$" and self.y or self.x, col) end end

-------------------------------------------------------
function DATA.new(it,   self) 
  self = l.is(DATA, {rows={}, cols=COLS.new(it())}) 
  for t in it do self:add(t) end  
  return self end

function DATA:add(t)
  l.push(self.rows, t)
  for _,cols in pairs{self.cols.x, self.cols.y} do
    for _,col in pairs(cols) do col:add(t[col.at]) end end end

-------------------------------------------------------
function SYM.new(s,n)
  return l.is(SYM, {txt=s, at=n, n=0, has={}, mode=nil, most=0, 
                    heaven=(s or ""):find"-$" and 0 or 1}) end

function SYM:add(x)
  if x ~= "?" then
    self.n = 1 + self.n
    self.has[x] = 1 + (self.has[x] or 0)
    if self.has[x] > self.most then self.most, self.mode = self.has[x], x end end end

--------------------------------------------------------
function NUM.new(s,n)
  return l.is(NUM, {txt=s, at=n, n=0, mu=0, m2=0, sd=0, lo=1E30, hi= -1E30}) end

function NUM:add(x,     d)
  if x ~= "?" then
    self.n  = 1 + self.n
    self.lo = math.min(x, self.lo)
    self.hi = math.max(x, self.hi)
    d       = x - self.mu
    self.mu = self.mu + d/self.n
    self.m2 = self.m2 + d*(x - self.mu)
    self.sd = (self.m2/(self.n - 1 + 1E-30))^0.5 end end

--------------------------------------------------------
function SYM:mid() return self.mode end
function NUM:mid() return self.mu end

function SYM:div() return l.entropy(self.has) end
function NUM:div() return self.sd end

function DATA:mids(cols) 
  return l.map(cols or self.cols.y, function(col) return l.rnd(col:mid()) end) end

--------------------------------------------------------
function SYM:bin(x) return x end
function NUM:bin(x) return math.min(the.bins, 1+((the.bins * self:norm(x))//1)) end

local BIN={}
function BIN.new(at,txt,lo,hi,n)
  return l.is(BIN,{at=at, txt=txt, lo=lo, hi=hi or lo, n=n or 0, ys={}}) end

function BIN:add(x,y)
  self.n  = self.n + 1
  self.lo = math.min(x, self.lo)
  self.hi = math.max(x, self.hi)
  self.ys[y] = 1 + (self.ys[y] or 0) end

function BIN:mergeable(other, small)
  both = BIN.new(self.at, self.txt, self.lo, other.hi, self.n + other.n)
  for _,ys in pairs{self.ys, other.ys} do
    for k,v in pairs(ys) do
      both.ys[k] = v + (both.ys[k] or 0) end end
  e1,e2,e12 = l.entropy(self.ys), l.entropy(other.ys), l.entropy(both.ys)
  if self.n < small or other.n < small then return both end
  if e12 <= (self.n*e1 + other.n*e2) / both.n then return both end end 
  
function DATA:bins(klasses,     all,tmp,n)
  all = {}
  for _,col in pairs(self.cols.x) do
    tmp, n = self:divides(col,klasses)
    for _,bin in pairs(col:merges(tmp, n/the.bins)) do l.push(all, bin) end end
  return all end

function DATA:divides(col,klasses)
  out,n = {},0
  for klass,rows in pairs(klasses) do
    for _,row in pairs(rows) do
      n += 1
      x = row[col.at]
      if x != "?" then 
        k = col:bins(x) 
        out[k] = out[k] or BIN.new(col.at, col.txt, x)
        out[k]:add(x,klass) end end end 
  return map(out, function(x) return x end), n end

function SYM:merges(x,_) return x end
function NUM:merges(b4,enough,    j,now,a,b,ab)
  j, now = 1, {}
  while j <=  len(b4) do
    a = b4[j]
    if j <  len(b4) then
      b = b4[j+1]
      ab = a:mergable(b,enough) 
      if ab then
        a = ab
        j = j+1 end end
    now += [a]
    j += 1 
  end
  if len(now) < len(b4) then return self:merges(now,enough) end
  for j=2,len(row) do
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
function l.is(x,y) 
  x.__tostring=l.o; x.__index=x; setmetatable(y,x); return y end

-- ### Lists
function l.by(s) return function(a,b) return a[s] < b[s] end end

function l.sort(t,fun) table.sort(t,fun); return t end

function l.push(t,x) t[1+#t]=x; return x end

function l.items(t,     k,n)
  k,n=0,#t
  return function() if k < n then k=k+1; return t[k] end end end

function l.map(t,f,     u) 
  u={};  for k,v in pairs(t) do u[1+#u] = f(v) end; return u end

function l.kap(t,f,     u) 
  u={};  for k,v in pairs(t) do u[1+#u] = f(k,v) end; return u end

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

-------------------------------------------------------
-- ## Start-up Actions
local function try(fun,     tmp,ok)
  tmp={}; for k,v in pairs(the) do tmp[k] = v end
  math.randomseed(the.seed)
  ok = xpcall(fun,function(err) print(tostring(debug.traceback())) end)
  for k,v in pairs(tmp) do the[k]=v end
  return ok end

-- Where to store the actions.
local eg={}

function eg.oo() l.oo(the) end

function eg.version() print("rulr version 0.2") end

function eg.csv()
  for t in l.csv(the.train)  do l.oo(t) end end

function eg.data()
  DATA.new(l.csv(the.train)) end

if   pcall(debug.getlocal, 4, 1) then-- if __name__ == "__main__":
else local todo = eg[arg[1]] and arg[1] or "version"
     try(eg[todo])
     l.rogues() end

return {the=the, lib=l,DATA=DATA,SYM=SYM,NUM=NUM,COLS=COLS}
