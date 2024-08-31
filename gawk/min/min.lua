#!/usr/bin/env lua
-- - Using as few dependent variables as possible...
-- - ...incrementally build models that recognize (best,rest) examples (where "best" 
--   can be  defined by  multiple goals). 
-- ### In this code:
-- - Settings are stored in `the` (and this variable is parsed from the `help` string at top of file).
-- - Test cases are stored in the `eg` table and test `eg.X` can be called at the command line using
--   "`./min.lua -X`" (optionally, with a command-line argument)
-- - In function arguments, 2 spaces denotes "start of optional args" and 
--   4 spaces denotes "start of local args".
-- - Class names are in UPPER CASE.
-- - atom = bool | str | int | float | "?"  
-- - row  = list[atom]
-- - rows = list[row]
local the,help = {},[[
min.lua : multiple-objective active learning
(c) 2024, Tim Menzies <timm@ieee.org>, BSD-2.

USAGE:
  chmod +x min.lua
  ./min.lua [OPTIONS] [ARGS]

OPTIONS:
  -b begin  int   initial samples = 4
  -B Break  int   max samples     = 30
  -c cut    int   items to sort   = 100
  -k k      int   Bayes param     = 1
  -m m      int   Bayes param     = 2
  -s seed   int   random seed     = 1234567891
  -t train  str   csv file        = ../../../moot/optimize/misc/auto93.csv
  -T Top    float best set size   = .5]]

local NUM,SYM,COLS,DATA = {},{},{},{}
local big,coerce,csv,down,fmt,gt,keys,lt,new,o,oo,pop,push,shuffle,sort,trim,up

-- -----------------------------------------------------------------------------------
-- ## Create

-- (int,str) --> SYM
function SYM:new(i,is) 
  i, is = i or 0, is or " "
  return new(SYM, {n=0, i=i, is=is, has={}, most=0, mode=nil}) end

-- (int,str) --> SYM
function NUM:new(i,is)
  i, is = i or 0, is or " "
  return new(NUM, {n=0, i=i, is=is, mu=0, sd=0, m2=0, lo=big, hi=-big,
                   goal = is:find"-$" and 0 or 1}) end

-- (lst[str]) --> COLS
function COLS:new(names,     all,x,y,col)
  all,x,y = {},{},{}
  for i,is in pairs(names) do
    col = push(all, (is:find"^[A-Z]" and NUM or SYM):new(i,is))
    if not is:find"X$" then
      push(is:find"[!+-]$" and y or x, col) end end
  return new(COLS, {names=names, all=all, x=x, y=y}) end

-- () --> DATA
function DATA:new() return new(DATA, {rows={}, cols=nil}) end

-- (rows) --> DATA 
function DATA:clone(rows) return DATA:new():from({self.cols.names}):from(rows) end

-- -----------------------------------------------------------------------------------
-- ## Update

-- (str) --> DATA
function DATA:csv(file)
  csv(file, function(_,row) self:add(row) end)
  return self end

-- (?list[row]) --> DATA
function DATA:from(rows)
  for _,row in pairs(rows or {}) do self:add(row) end
  return self end

-- (row) --> nil
function DATA:add(row) 
  if   self.cols 
  then push(self.rows,self.cols:add(row)) 
  else self.cols=COLS:new(row) end end

-- (row) --> row
function COLS:add(row)
  for _,cols in pairs{self.x, self.y} do
    for _,col in pairs(cols) do
      col:add( row[col.i] ) end end
  return row end

-- (atom) --> nil
function NUM:add(x,    d)
  if x ~= "?" then
    self.n  = self.n + 1
    d       = x - self.mu
    self.mu = self.mu + d / self.n
    self.m2 = self.m2 + d * (x - self.mu)
    self.sd = self.n < 2 and 0 or (self.m2/(self.n - 1))^.5 
    if x > self.hi then self.hi = x end
    if x < self.lo then self.lo = x end end end  

-- (atom) --> nil
function SYM:add(x,  n)
  if x ~= "?" then
    n           = n or 1
    self.n      = n + self.n 
    self.has[x] = n + (self.has[x] or 0) 
    if self.has[x] > self.most then
      self.most, self.mode = self.has[x], x end end end

-- -----------------------------------------------------------------------------------
-- ## Query

-- ("?" | number) --> 0..1
function NUM:norm(x)
  return x=="?" and x or (x - self.lo) / (self.hi - self.lo + 1/big) end

-- -----------------------------------------------------------------------------------
-- ## Goals

-- (row) --> 0..1
function DATA:chebyshev(row,    tmp,d)
  d=0; for _,col in pairs(self.cols.y) do
         tmp = math.abs(col.goal - col:norm(row[col.i]))
         if tmp > d then d = tmp end end
  return d end

-- () --> DATA
function DATA:shuffle()
  self.rows = shuffle(self.rows)
	return self end

-- () --> DATA
function DATA:sort(    fun)
  fun = function(row) return self:chebyshev(row) end
  self.rows = sort(self.rows, function(a,b) return fun(a) < fun(b) end)
  return self end

-- () --> DATA,DATA
function DATA:bestRest(      best,rest)
  self:sort()
  best,rest = self:clone(), self:clone()
  for i,row in pairs(self:sort().rows) do
    (i <= (#self.rows)^the.Top and best or rest):add(row) end
  return best,rest end

-- -----------------------------------------------------------------------------------
-- ## Bayes

-- (atom,number) --> number
function SYM:like(x, prior)
  return ((self.has[x] or 0) + the.m*prior)/(self.n +the.m) end

-- (atom, any...) --> number
function NUM:like(x,...)
  local sd, mu = self.sd, self.mu
  if sd==0 then return x==mu and 1 or 1E-32 end
  return math.exp(-.5*((x - mu)/sd)^2) / (sd*((2*math.pi)^0.5)) end

-- (row, int, int) --> number
function DATA:like(row, n, nClasses,     col,prior,out,v,inc)
  prior = (#self.rows + the.k) / (n + the.k * nClasses)
  out   = math.log(prior)
  for _,col in pairs(self.cols.x) do
    v = row[col.i]
    if v ~= "?" then
      inc = col:like(v,prior)
      if inc > 0 then out = out + math.log(inc) end end end
  return out end

-- (rows, function) --> row
function DATA:acquire(rows, score,      todo,done)
  todo, done = {},{}
  for i,t in pairs(rows or self.rows) do 
    push(i <= the.begin and done or todo, t) end
  while #done < the.Break do
    todo = self:guessNextBest(todo, done, score or function(B,R) return B - R end) 
    push(done, pop(todo)) end
    done = self:clone(done):sort().rows 
  return done[#done] end

-- (rows, rows, function) --> row
function DATA:guessNextBest(todo,done,score,     best,rest,fun,tmp)
  best,rest = self:clone(done):bestRest()
  fun = function(t) return score(best:like(t,#done,2),rest:like(t,#done,2)) end
  tmp, out  = {},{}
  for i,t in pairs(todo) do push(tmp, {t, i <= the.cut and fun(t) or -big}) end
  for _,one in pairs(sort(tmp, lt(1))) do push(out, one[2]) end
  return out end

-- -----------------------------------------------------------------------------------
-- ## Lib

-- --> number
big = 1E32
-- (table) --> any
pop = table.remove
-- (str) --> string
fmt = string.format

-- (t1,t2) --> t2 
function new(klass,obj)
  klass.__index=klass; klass.__tostring=o; return setmetatable(obj,klass) end

-- (t,any) --> t
function push(t,x)   t[1+#t]=x; return x end

-- (t,?function) --> t
function sort(t,  fun) table.sort(t,fun); return t end

function lt(key)   return function(a,b) return a[key] < b[key] end end
function gt(key)   return function(a,b) return a[key] > b[key] end end
function up(fun)   return function(a,b) return fun(a) > fun(b) end end
function down(fun) return function(a,b) return fun(a) < fun(b) end end

function keys(t,    u) 
  u={}; for k,_ in pairs(t) do push(u,k) end return sort(u) end   

function shuffle(t,    j)
  for i = #t, 2, -1 do j = math.random(i); t[i], t[j] = t[j], t[i] end
  return t end

function coerce(s,     fun)   
  fun = function(s) return s=="true" and true or s ~= "false" and s end
  return math.tointeger(s) or tonumber(s) or fun(trim(s)) end

function csv(file,fun,      src,s,cells,n)
  function cells(s,    t)
    t={}; for s1 in s:gmatch"([^,]+)" do t[1+#t] = coerce(s1) end; return t end
  src = io.input(file)
  n   = -1
  while true do
    s = io.read()
    if s then n=n+1; fun(n,cells(s)) else return io.close(src) end end end

function trim(s)  return s:match"^%s*(.-)%s*$" end

function o(x,     list,hash)
  list= function(t) for _,v in pairs(x) do t[1+#t] = o(v) end; return t end
  hash= function(t) for _,k in pairs(keys(x)) do 
                      if   not o(k):find"^_" 
                      then t[1+#t] = fmt(":%s %s", k, o(x[k])) end end 
                    return t end
  if type(x) == "number" then return fmt("%.3g",x) end
  if type(x) ~= "table"  then return tostring(x)   end
  return "{" .. table.concat(#x>0 and list{} or hash{}, " ") .. "}" end

function oo(x) print(o(x)) end

-- -----------------------------------------------------------------------------------
-- ## Examples

local eg = {}

function eg.train(x) the.train = x end

function eg.seed(x) the.seed = coerce(x); math.randomseed(the.seed) end

function eg.sort(_,     t)
  t = sort({10,1,2,3,1,4,1,1,2,4,2,1}, function(a,b) return a>b end)
  assert(t[1]==10, "sort") end

function eg.csv(_,     fun) 
  fun = function(n,t) if (n % 60) == 0 then print(n, o(t)) end end
  csv(the.train, fun) end

function eg.data(_,      d)
  d = DATA:new():csv(the.train) 
  for _,col in pairs(d.cols.y) do oo(col) end end

function eg.bayes(_,      d,fun)
  d   = DATA:new():csv(the.train) 
  fun = function(t) return d:like(t,1000,2) end
  for n,t in pairs(sort(d.rows, down(fun))) do
   if n % 30 == 0 then print(n, fun(t)) end end end

function eg.cheb(_,      d,num)
  d   = DATA:new():csv(the.train) 
  num = NUM:new()
  for _,t in pairs(d.rows) do num:add(d:chebyshev(t)) end
  print(num.mu, num.sd) end

function eg.acq(_,      d,num)
  d   = DATA:new():csv(the.train) 
  num = NUM:new()
	for i=1,20 do
	  num:add( d:chebyshev(d:shuffle():acquire()) ) end
	print(num.mu) end

function eg.push(_) os.execute("git commit -am saving; git push; git status") end
function eg.pdf(_)  os.execute("make -B ~/tmp/min.pdf; open ~/tmp/min.pdf") end
function eg.doc(_)  os.execute(
  "pycco -d ~/tmp min.lua; echo 'p {text-align:right;}' >> ~/tmp/pycco.css") end

function eg.the(_) oo(the) end
function eg.h(_) print("\n" ..help) end

-- -----------------------------------------------------------------------------------
-- ## Start

help:gsub("\n%s+-%S%s+(%S+)[^=]+=%s+(%S+)", function(k,v) the[k] = coerce(v) end)
math.randomseed(the.seed)

if arg[0]:find"min.lua" 
then for i,s in pairs(arg) do 
       s = s:sub(2)
       if eg[s] then eg[s]( arg[i+1] ) end end end 
