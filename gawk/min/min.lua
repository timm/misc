#!/usr/bin/env lua
local big = 1E32

local the = {
  begin = 4,
  Break = 30,
  cut   = 100,
  k     = 1,
  m     = 2,
  seed  = 1234567891,
  train = "../../../moot/optimize/misc/auto93.csv",
  Top   = .5
}
 
-- ----------------------------------------------------------------------------------
local pop = table.remove

local function lt(key)     return function(a,b) return a[key] < b[key] end end
local function gt(key)     return function(a,b) return a[key] > b[key] end end
local function up(fun)     return function(a,b) return fun(a) > fun(b) end end
local function down(fun)   return function(a,b) return fun(a) < fun(b) end end
local function sort(t,fun) table.sort(t,fun); return t end
local function push(t,x)   t[1+#t]=x; return x end

local function keys(t,    u) 
  u={}; for k,_ in pairs(t) do push(u,k) end
  return sort(u) end   

local function shuffle(t,    j)
  for i = #t, 2, -1 do j = math.random(i); t[i], t[j] = t[j], t[i] end
  return t end

local function trim(s)  return s:match"^%s*(.-)%s*$" end
local function coerce1(s) return s=="true" and true or s ~= "false" and s end
local function coerce(s)   
  return math.tointeger(s) or tonumber(s) or coerce1(trim(s)) end

local function csv(file,fun,      src,s,cells,n)
  function cells(s,    t)
    t={}; for s1 in s:gmatch"([^,]+)" do t[1+#t] = coerce(s1) end; return t end
  src = io.input(file)
  n   = -1
  while true do
    s = io.read()
    if s then n=n+1; fun(n,cells(s)) else return io.close(src) end end end

local fmt = string.format

local function _table2string(x,t)
  if   #x > 0 
  then for k,v in pairs(x) do t[1+#t] = o(v) end
  else for _,k in keys(x)  do 
         if not o(k):find"^_" then t[1+#t] = fmt(":%s %s", k, o(x[k])) end end end
  return t end

local function o(x,    t)
  if type(x) == "number" then return fmt("%.3g",x) end
  if type(x) ~= "table"  then return tostring(x) end
  return "{" .. table.concat( _table2string(x,{}), " ") .. "}" end

local function oo(x) print(o(x)) end

local function new(klass,obj)
  klass.__index=klass; klass.__tostring=o; return setmetatable(obj,klass) end

-- ----------------------------------------------------------------------------------
local NUM,SYM,COLS,DATA = {},{},{},{}

function SYM:new(i,is)
  i, is = i or 0, is or " "
  return new(SYM, {n=0, i=i, is=is, has={}, most=0, mode=nil}) end

function NUM:new(i,is)
  i, is = i or 0, is or " "
  return new(NUM, {n=0, i=i, is=is, mu=0, sd=0, m2=0, lo=big, hi=-big,
                   goal = is:find"-$" and 0 or 1}) end

function COLS:new(names,     all,x,y,col)
  all,x,y = {},{},{}
  for i,is in pairs(names) do
    col = push(all, (is:find"^[A-Z]" and NUM or SYM):new(i,is))
    if not is:find"X$" then
      push(is:find"[!+-]$" and y or x, col) end end
  return new(COLS, {names=names, all=all, x=x, y=y}) end

function DATA:new() 
  return new(DATA, {rows={}, cols=nil}) end

function DATA:clone(rows)
  return DATA:new():from(rows) end

function DATA:csv(file)
  csv(file, function(n,row) self:add(row) end)
  return self end

function DATA:from(rows)
  for row in pairs(rows or {}) do self:add(row) end
  return self end

-- ----------------------------------------------------------------------------------
-- Misc methods

function NUM:norm(x)
  return x=="?" and x or (x - self.lo) / (self.hi - self.lo + 1/big) end

-- ----------------------------------------------------------------------------------
function DATA:add(row) 
  if   self.cols 
  then push(self.rows, self.cols:add(row))
  else self.cols = COLS:new(row) end end

function COLS:add(row)
  for _,cols in pairs{self.x, self.y} do
    for _,col in pairs(cols) do
      col:add( row[col.i] ) end end
  return row end

function NUM:add(x,    d)
  if x ~= "?" then
    self.n  = self.n + 1
    d       = x - self.mu
    self.mu = self.mu + d / self.n
    self.m2 = self.m2 + d * (x - self.mu)
    self.sd = self.n < 2 and 0 or (self.m2/(self.n - 1))^.5 
    if x > self.hi then self.hi = x end
    if x < self.lo then self.lo = x end end end  

function SYM:add(x,  n)
  if x ~= "?" then
    n           = n or 1
    self.n      = n + self.n 
    self.has[x] = n + (self.has[x] or 0) 
    if self.has[x] > self.most then
      self.most, self.mode = self.has[x], x end end end 

-- ----------------------------------------------------------------------------------
function DATA:chebyshev(row,    tmp,d)
  d=0; for _,col in pairs(self.cols.y) do
         tmp = math.abs(col.goal - col:norm(row[col.i]))
         if tmp > d then d = tmp end end
  return d end

function DATA:shuffle()
  self.rows = shuffle(self.rows)
	return self end

function DATA:sort(    fun)
  fun = function(row) return self:chebyshev(row) end
  self.rows = sort(self.rows, function(a,b) return fun(a) < fun(b) end)
  return self end

function DATA:bestRest(      best,rest)
  self:sort()
  best,rest = self:clone(), self:clone()
  for i,row in pairs(self:sort().rows) do
    (i <= (#self.rows)^the.Top and best or rest):add(row) end
  return best,rest end

-- ----------------------------------------------------------------------------------
function SYM:like(x, prior)
  return ((self.has[x] or 0) + the.m*prior)/(self.n +the.m) end

function NUM:like(x,...)
  local sd, mu = self.sd, self.mu
  if sd==0 then return x==mu and 1 or 1E-32 end
  return math.exp(-.5*((x - mu)/sd)^2) / (sd*((2*math.pi)^0.5)) end

function DATA:like(row, n, nClasses,     col,prior,out,v,inc)
  prior = (#self.rows + the.k) / (n + the.k * nClasses)
  out   = math.log(prior)
  for _,col in pairs(self.cols.x) do
    v = row[col.i]
    if v ~= "?" then
      inc = col:like(v,prior)
      if inc > 0 then out = out + math.log(inc) end end end
  return out end

function DATA:acquire(rows, score,      todo,done)
  todo, done = {},{}
  for i,row in pairs(rows or self.rows) do 
    push(i <= the.begin and done or todo, row) end
  while #done < the.Break do
    todo = self:guessNextBest(todo, done, score or function(B,R) return B - R end) 
    push(done, pop(todo)) end
    done = self:clone(done):sort().rows 
  return done[#done] end

function DATA:guessNextBest(todo,done,score,     best,rest,fun,tmp)
  best,rest = self:clone(done):bestRest()
  fun = function(row) return score(best:like(row,#done,2),rest:like(row,#done,2)) end
  tmp = {}
  for i,row in pairs(todo) do push(tmp, {row, i <= the.cut and fun(row) or -big}) end
  todo = {}
  for _,one in pairs(sort(tmp, lt(1))) do push(todo, one[2]) end
  return todo end

-- ---------------------------------------------------------------------------------
local eg = {}

function eg.train(x) the.train = x end

function eg.seed(x) the.seed = coerce(x); math.randomseed(the.seed) end

function eg.sort(_,     t)
  t = sort({10,1,2,3,1,4,1,1,2,4,2,1}, function(a,b) return a>b end)
  assert(t[1]==10, "sort") end

function eg.csv(_,     fun) 
  fun = function(n,row) if (n % 60) == 0 then print(n, o(row)) end end
  csv(the.train, fun) end

function eg.data(_,      d)
  d = DATA:new():csv(the.train) 
  for _,col in pairs(d.cols.y) do oo(col) end end

function eg.bayes(_,      d,fun)
  d   = DATA:new():csv(the.train) 
  fun = function(row) return d:like(row,1000,2) end
  for n,row in pairs(sort(d.rows, down(fun))) do
   if n % 30 == 0 then print(n, fun(row)) end end end

function eg.cheb(_,      d,num)
  d   = DATA:new():csv(the.train) 
  num = NUM:new()
  for _,row in pairs(d.rows) do num:add(d:chebyshev(row)) end
  print(num.mu, num.sd) end

function eg.acq(_,      d,num)
  d   = DATA:new():csv(the.train) 
  num = NUM:new()
	for i=1,20 do
	  num:add( d:chebyshev(d:shuffle():acquire()) ) end
	print(num.mu) end

function eg.the(_) oo(the) end

-- ----------------------------------------------------------------------------------
math.randomseed(the.seed)

for i,s in pairs(arg) do 
  s = s:sub(2)
  if eg[s] then eg[s]( arg[i+1] ) end end
