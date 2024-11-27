#!/usr/bin/env lua
-- vim: set tabstop=2 shiftwidth=2 expandtab :
local help=[[
kah.lua : a tiny AI workbench
(c) 2024, Tim Menzies <timm@ieee.org>, MIT license

USAGE:
  ./nb.lua [OPTIONS] [x.csv]

OPTIONS:
  --the         show options
  --help        show help
  --csv   x.csv demo: printing rows from csv
  --cols        demo: show cols initialized from row names
  --like  x.csv demo: show row liklihood calc]]

local abs,cos,exp,log,min = math.abs, math.cos, math.exp, math.log, math.min
local max, sqrt, pi,R = math.max, math.sqrt, math.pi, math.random
local adds,any,bootstrap,cliffs,coerce,csv,gt,keysort,lt,many,map
local new,normal,o,pop,push,shuffle,same,sort,split,printf,sum
local l, eg, the = {}, {}, {
  acquire= "exploit",
  big   = 1E32,
  boots = 256,
  cliffs= 0.197,
  conf  = 0.05,
  enough = 500,
  k     = 1,
  m     = 2,
  p     = 2,
  seed  = 1234567891,
  start = 4,
  stop  = 30,
  test  = 0.4}

-------------------------------------------------------------------------------
local Num,Sym,Cols,Data = {},{},{},{}

function Num:new(txt,at) 
  return new(Num, {at=at or 0, txt=txt or "", n=0, 
                   mu=0, m2=0, sd=0, lo = math.huge, hi= -math.huge,
                   goal = (txt or ""):find"-$" and 0 or 1}) end

function Sym:new(txt,at) 
  return new(Sym, {at=at or 0, txt=txt or "", n=0, has={},most=0,mode=nil}) end

function Cols:new()
  return new(Cols, {names={},all={}, y={}, x={}, klass=nil,initialized=false}) end

function Data:new() 
  return new(Data, {rows={}, cols=Cols:new()}) end

function Data:clone(rows)
   return Data:new():add(self.cols.names):adds(rows or {}) end

-------------------------------------------------------------------------------
function Sym:add(x,  n)
  n = n or 1
  self.n = self.n + n
  self.has[x] = n + (self.has[x] or 0)
  if self.has[x] > self.most then self.most, self.mode=self.has[x], x end end 

function Num:add(x,    d)
  self.n  = self.n + 1
  self.lo = min(x, self.lo)
  self.hi = max(x, self.hi)
  d = x - self.mu
  self.mu = self.mu + d/self.n
  self.m2 = self.m2 + d*(x - self.mu)
  self.sd = self.n < 2 and 0 or sqrt(self.m2 / (self.n - 1)) end 

function Data:adds(src)
  if type(src)=="table" then return adds(src,self) end
  for row in csv(src) do self:add(row) end 
  return self end
    
function Data:add(row)
  if   self.cols.initialized
  then push(self.rows, self.cols:add(row))
  else self.cols:initialize(row) end 
  return self end

function Cols:add(row)
  for at,x in pairs(row) do
    if x ~="?" then
      self.all[at]:add(x) end end
  return row end 

function Cols:initialize(names,    col)
  self.names = names
  for at,s in pairs(self.names) do
    col = push(self.all, (s:find"^[A-Z]" and Num or Sym):new(s,at))
    if s:find"!$" then self.klass = col end
    push(s:find"[!+-]$" and self.y or self.x, col) end 
  self.initialized = true 
  return self end

-------------------------------------------------------------------------------
     
function Sym:div() 
  return -sum(self.has, function(n) return n/self.n * log(n/self.n,2) end) end

function Num:div() return self.sd end

function Num:norm(x)
  return x=="?" and x or (x-self.lo) / (self.hi - self.lo + 1/the.big) end

function Num.pooledSd(i,j)
  return sqrt(((i.n-1)*i.sd^2 + (j.n-1)*j.sd^2)/(i.n+j.n-2)) end

function Num.delta(i,j)
  return abs(i.mu - j.mu) / ((1E-32 + i.sd^2/i.n + j.sd^2/j.n)^.5) end

function Data:klass(row) return row[self.cols.klass.at] end

-------------------------------------------------------------------------------
function Sym:dist(a,b) 
  return  a=="?" and b=="?" and 1 or  (a==b and 0 or 1) end

function Num:dist(a,b)
  a,b = self:norm(a), self:norm(b)
  a = a ~= "?" and a or (b<0.5 and 1 or 0)
  b = b ~= "?" and b or (a<0.5 and 1 or 0)
  return abs(a-b) end

function Data:xdist(row1,row2,    DIST) 
  DIST= function(col) return col:dist(row1[col.at], row2[col.at])^the.p  end
  return (sum(self.cols.x, DIST) / #self.cols.x) ^ (1/the.p) end

function Data:neighbors(row1,  rows,      X) 
  X= function(row2) return self:xdist(row1,row2) end
  return keysort(rows or self.rows, X) end

function Data:ydist(row,     Y)
  Y= function(col) return (col:norm(row[col.at]) - col.goal)^2 end
  return sqrt(sum(self.cols.y, Y)/#(self.cols.y)) end

function Data:ydists(  rows,     Y)
  Y= function(row) return self:ydist(row) end
  return keysort(rows or self.rows, Y) end

-------------------------------------------------------------------------------
-- `like(x:atom, ?prior: float) -> float`  
-- Returns nil if `x` is a missing value.  
function Sym:like(x,  prior,    v,tmp) --> num
  if x~="?" then
    return ((self.has[x] or 0) + the.m*prior) / (self.n + the.m) end end

-- Ditto. Ignores second argument (the `prior`).
-- For an explanation of this function, see 
-- https://chatgpt.com/share/6744b232-a680-8010-a20b-99a0d5fdd364
function Num:like(x,_,     v,tmp)
  if x~="?" then
    v = self.sd^2 + 1/the.big
    tmp = exp(-1*(x - self.mu)^2/(2*v)) / (2*pi*v) ^ 0.5
    return max(0,min(1, tmp + 1/the.big)) end end

function Data:loglike(row,  nall,nh,    prior,LIKE,LOG)
  LIKE = function(col) return LOG( col:like(row[col.at], prior) ) end
  LOG  = function(x)   return x and x>0 and log(x) or 0 end
  prior= (#self.rows + the.k) / (nall + the.k*nh)
  return LOG(prior) + sum(self.cols.x, LIKE) end

function Data.classify(row, datas,    most,out,nh,nall,tmp)
  most,nh,nall = -math.huge,0,0
  for _,data in pairs(datas) do nh = nh + 1; nall = nall + #data.rows end
  for k,data in pairs(datas) do
     tmp = data:loglike(row,nall,nh)
     if tmp > most then most,out = tmp,k end end
  return out,most end

function Data:guess(sortp) 
  local ACQ,Y,B,R,BR,test,train,done,todo,best,rest,rows
  rows = shuffle(self.rows)
  ACQ= {
    exploit = function(b,r) return b / r end,
    explore = function(b,r) return (b + r)/(abs(b-r) + 1/the.big) end,
    adapt   = function(b,r) local w = (1 - #done/the.stop)  
                             return (b+r*w) / (abs(b*w - r) + 1/the.big) end
  }
  Y         = function(row) return self:ydist(row) end
  B         = function(row) return exp(best:loglike(row, #done, 2)) end 
  R         = function(row) return exp(rest:loglike(row, #done, 2)) end 
  BR        = function(row) return -ACQ[the.acquire](B(row), R(row)) end
  train,test= split(shuffle(rows), min(the.enough, the.test*#rows))
  done,todo = split(train, the.start) 
  while true do
    done = keysort(done, Y) 
    if #done > the.stop or #todo < 5 then break end 
    best,rest = self:clone(), self:clone()
    for j,row in pairs(done) do (j<=sqrt(#done) and best or rest):add(row) end
    todo = keysort(todo,BR)             
    for _=1,3 do push(done, pop(todo,1)); push(done, pop(todo)) end end
  return done, (sortp and keysort(test,BR) or test) end   

-------------------------------------------------------------------------------
function Num:keeps(t)
  for _,x in pairs(t) do self:keep(x) end
  return self end

function Num:keep(x)
  self._nums = self._nums or {}
  push(self._nums, x)
  self:add(x) end

function Num.merge(i,j,  eps,     k)
  if abs(i.mu - j.mu) < (eps or 0) or same(i._nums, j._nums) then
    k = Num:new(i.txt,i.at)
    k._nums = {}
    for _,nums in pairs{i._nums, j._nums} do
      for _,n in pairs(nums) do k:keep(n) end end end 
  return k end

function Num.merges(nums,  maxp,eps,     t,merged)
  for _,num in pairs(sort(nums, maxp and gt"mu" or lt"mu")) do
    if   t 
    then merged = num:merge(t[#t],eps)
         if merged then t[#t] = merged else push(t,num) end
    else t= {num} end 
    num.rank = string.format("%c",96+#t) end
  return t end

function l.same(x,y)
  return l.cliffs(x,y) and l.bootstrap(x,y) end

function l.cliffs(xs,ys,     lt,gt,n)
  lt,gt,n = 0,0,0
  for _,x in pairs(xs) do
      for _,y in pairs(ys) do
        n = n + 1
        if y > x then gt = gt + 1 end
        if y < x then lt = lt + 1 end end end
  return abs(gt - lt)/n <= the.cliffs end -- 0.195 
      
-- Taken from non-parametric significance test From Introduction to Bootstrap,
-- Efron and Tibshirani, 1993, chapter 20. https://doi.org/10.1201/9780429246593
-- Checks how rare are  the observed differences between samples of this data.
-- If not rare, then these sets are the same.
function l.bootstrap(y0,z0,         x,y,z,yhat,zhat,n,b)
  z,y,x= adds(z0), adds(y0), adds(y0,adds(z0))
  yhat = map(y0, function(y1) return y1 - y.mu + x.mu end)
  zhat = map(z0, function(z1) return z1 - z.mu + x.mu end)
  n    = 0
  for i=1, the.boots do 
    if adds(many(yhat)):delta(adds(many(zhat))) > y:delta(z) then n=n+1 end end
  return n / the.boots >= the.conf end

-------------------------------------------------------------------------------
function l.normal(mu,sd) --> (num, num) --> 0..1
  return (mu or 0) + (sd or 1) * sqrt(-2*log(R())) * cos(2*pi*R()) end

function l.printf(...) print(string.format(...)) end

function l.adds(t,it) 
  for _,x in pairs(t) do 
    it = it or (type(x)=="number" and Num or Sym):new()
    it:add(x) end
  return it; end

function l.any(t)  
  return t[R(#t)] end

function l.many(t,n,  u) 
  u={}; for i=1,(n or #t) do u[i] = any(t) end; return u end

function l.push(t,x) t[1+#t]=x; return x end

function l.pop(t,n) return table.remove(t,n) end

function l.shuffle(t,    k) 
  for j = #t,2,-1 do k=math.random(j); t[j],t[k] = t[k],t[j] end; return t end

function l.sort(t,FUN) table.sort(t,FUN); return t end

function l.lt(x) return function(a,b) return a[x] < b[x] end end
function l.gt(x) return function(a,b) return a[x] > b[x] end end

function l.map(t,FUN,   u) 
  u={}; for _,v in pairs(t) do u[1+#u]=FUN(v) end; return u end

function l.sum(t,FUN,   n)
  n=0; for _,x in pairs(t) do n=n+FUN(x) end; return n end

function l.split(t, n,     u,v) 
  u,v={},{}; for j,x in pairs(t) do l.push(j<=n and u or v,x) end; return u,v end

function l.keysort(t,FUN,       DECORATE, UNDECORATE) 
  DECORATE   = function(x) return {FUN(x),x} end
  UNDECORATE = function(x) return x[2] end
  return map(sort(map(t,DECORATE),lt(1)), UNDECORATE) end

function l.coerce(s,     FUN,TRIM) 
  TRIM= function(s) return s:match"^%s*(.-)%s*$" end
  FUN = function(s) return s=="true" and true or s ~= "false" and s end
  return math.tointeger(s) or tonumber(s) or FUN(TRIM(s)) end

function l.csv(file,     src) 
  if file ~="-" then src=io.input(file) end
  return function(     s,t)
    s = io.read()
    if s 
    then t={}; for s1 in s:gmatch"([^,]+)" do t[1+#t]=l.coerce(s1) end; return t 
    else if src then io.close(src) end end end end

function l.o(x,  n,        t,FMT,NUM,LIST,DICT,PUB) 
  t = {}
  n = n or 0
  assert(n<50,"recursive string error")
  FMT = string.format
  NUM = function() return x//1 == x and tostring(x) or FMT("%.3g",x) end
  LIST= function() for k,v in pairs(x) do t[1+#t] = l.o(v,n+1) end end
  DICT= function() for k,v in pairs(x) do 
                     if PUB(k) 
                     then t[1+#t] = FMT(":%s %s",k, l.o(v,n+1)) end end end
  PUB= function(s) return  not tostring(s):find"^_" end
  if type(x) == "number" then return NUM() end 
  if type(x) ~= "table"  then return tostring(x) end
  if #x>0 then LIST() else DICT(); table.sort(t) end
  return "{" .. table.concat(t, " ") .. "}" end

function l.new(kl,t)
  kl.__index=kl; kl.__tostring = l.o; return setmetatable(t,kl) end


-------------------------------------------------------------------------------
function eg.help(_) print("\n"..help.."\n") end
function eg.the(_)  print(o(the)) end
function eg.stop(x)  the.stop = coerce(x) end

function eg.num(_, n) 
  n = Num:new(); for _=1,100 do n:add(normal(20,2)) end
  assert(abs(n.sd - 1.96) < 0.01) end

function eg.sym(_, s) 
  s = Sym:new(); for _,x in pairs{"a","a","a","a","b","b","c"} do s:add(x) end
  assert(abs(1.379 - s:div()) < 0.01) end

function eg.csv(f) for row in csv(f) do print(o(row)) end end

function eg.data(f)
  for _,col in pairs(Data:new():adds(f).cols.all) do 
    print(o(col)) end end

function eg.cols(_,    names)
  names={"cylinders", "Displacement", "HorsepowerX", 
         "Weight-", "Acceleration+", "Model", "origin", "Mpg+"}
  for _,col in pairs(Cols:new():initialize(names).all) do print(o(col)) end end

function eg.like(f,    d)
  d = Data:new():adds(f)
  for k,row in pairs(shuffle(d.rows)) do 
    if k>20 then break else print(row[#row],d:loglike(row,1000,2)) end end end

function eg.ydists(f,    d)
  d = Data:new():adds(f)
  for k,row in pairs(d:ydists()) do
    if k>20 then break else print(row[#row],d:ydist(row)) end end end

function eg.guess(f,     done,test,d,n,train,trains,tests,N)
  d = Data:new():adds(f)
  n = adds(sort(map(d.rows, function(row) return d:ydist(row) end)))
  N = function (x)  return (x-n.lo)/n.sd end
  trains,tests = Num:new(),Num:new()
  for _=1,20 do
    train,test = d:guess(true)
    trains:add(N(d:ydist(train[1])))
    tests:add(N(d:ydist(test[1]))) end
  printf("%s, %.2f, %.2f,  %.2f, %.2f", f:gsub("^.*/",""), 
         N(n.mu), trains.mu, tests.mu, tests.mu - trains.mu)  end 

function eg.stats(   t,u,d,Y,n1,n2)
  print("d\t cliff\tboot\tcohen")
  Y = function(s) return s and "y" or "." end
  d= 1
  while d< 1.3 do
    t={}; for i=1,100 do t[1+#t] = normal(10,1) + normal(15,4)^2 end 
    u={}; for i,x in pairs(t) do  u[i] = x*d end
    d=d*1.02
    n1,n2 = adds(t), adds(u)
    print(string.format("%.3f\t%s\t%s\t%s", 
                        d, Y(cliffs(t,u)), Y(bootstrap(t,u)), 
                           Y(abs(n1.mu - n2.mu) < .35*n1:pooledSd(n2)))) end end

function eg.keeps(   t)
  t= {Num:new("x1"):keeps{0.34, 0.49 ,0.51, 0.6},
      Num:new("x2"):keeps{0.6  ,0.7 , 0.8 , 0.89},
      Num:new("x3"):keeps{0.13 ,0.23, 0.38, 0.38},
      Num:new("x4"):keeps{0.6  ,0.7,  0.8 , 0.9},
      Num:new("x5"):keeps{0.1  ,0.2,  0.3 , 0.4}} 
  Num.merges(t,true)
  for _,num in pairs(t) do
    print(num.mu, num.rank) end end

-------------------------------------------------------------------------------
adds, any, bootstrap          = l.adds, l.any, l.bootstrap
cliffs, coerce, csv, gt       = l.cliffs, l.coerce, l.csv, l.gt
keysort, lt, many, map, new   = l.keysort, l.lt, l.many, l.map, l.new
normal,o, pop,push, shuffle   = l.normal, l.o, l.pop, l.push, l.shuffle
same,sort, split, sum, printf = l.same, l.sort, l.split, l.sum, l.printf

math.randomseed(the.seed)

if arg[0]:find"nb.lua" then
  for j,s in pairs(arg) do
     s=s:gsub("^[-][-]","")
     if eg[s] then  eg[s](arg[j+1]) end 
     math.randomseed(the.seed) end end

return {Num=Num, Sym=Sym, Data=Data, the=the, lib=l} 
