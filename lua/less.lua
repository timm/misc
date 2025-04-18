-- vim: set ts=2 sw=2 sts=2 et:

-- all the "top" vars (used across the whole module)
local THE = {
  about   = {what = "new.lua", 
             why  = "simple inference", 
             when = "(c) 2025, MIT License", 
             who  = "Tim Menzies"}, 
  rseed   = 1234567891,
  data    = "../data/auto93.csv", 
  k       = 1,                   
  m       = 2,                  
  p       = 2,                 
  samples = 256               
}

local BIG = 1E32
local DATA,SYM,NUM,COLS = {},{},{},{}

------------------------------------------------------------------------------
local pi,abs,exp,log,max,min,random,randomseed
pi  = math.pi
abs = math.abs
exp = math.exp
log = math.log
max = math.max
min = math.min
random = math.random
randomseed = math.randomseed

local any, fmt,new,push

function any(t) return t[random(#t)] end

fmt=string.format

function new(isa,t) isa.__index = isa; return setmetatable(t,isa) end

function push(t,x) t[1+#t]=x; return x end 

local map,mop,sum

function map(t,f,   u) 
  u={}; for _,x in pairs(t) do u[1+#u]= f(x) end; return u end 

function mop(t,f,o,...)
  local u={}
  for _,x in pairs(t) do u[1+#u]=getmetatable(o)[f](o,...) end; return u end 

function sum(t,f,   n) 
  n=0; for _,x in pairs(t) do n = n + f(x) end; return n end 

local lt,sort,keysort

function lt(x,    _num) 
  _num = function(x) return x=="?" and BIG or x end
  return function(t,u) return _num(t[x]) < _num(u[x]) end end

function sort(t) table.sort(t); return t end

function keysort(a,  _fun)
  local _decorate   = function(x) return {_fun(x),x} end
  local _undecorate = function(x) return x[2] end
  return  map(sort(map(a, _decorate), lt(1)), _undecorate) end

------------------------------------------------------------------------------
local csv,csvFile

function csv(s,    t)
  t={}; for s1 in s:gmatch"([^,]+)" do t[1+#t]=s1 end; return t end 

function csvFile(src)
  src = io.input(src)
  return function(    s)
    s = io.read()
    return s and csv(s) or io.close(src) end end 

------------------------------------------------------------------------------
local odist,olist,o,out

function odict(x,    t)
  t={}; for k,v in pairs(x) do t[1+#t]=fmt(":%s %s",k,o(v)) end
  return sort(t) end

function olist(x,t)
  t={}; for _,v in pairs(x) do t[1+#t]=o(v) end; return t end

function o(x)
  if type(x)=="number" then return fmt(x//1 == x and "%s" or "%.3g",x) end
  if type(x)~="table"  then return tostring(x) end
  return "{".. table.concat(#x>0 and olist(x) or odist(x)," ") .."}" )) end 

function out(x) print(o(x)); return x end

------------------------------------------------------------------------------
local Data,Num,Sym,Cols

function Data(src,    self)
  local self = new(DATA,{cols=nil, rows={}})
  if   type(src)=="string" 
  then for   t in csvFile(src)     do self:add(t) end
  else for _,t in pairs(src or {}) do self:add(t) end end
  return self end 

function Num(s,n) 
  return new(NUM, {txt=s or " ", at=n or 1, n=0,
                   mu=0, m2=0, lo=BIG, hi=-BIG,
                   goal=tostring(s or " "):find"-$" and 0 or 1}) end

function Sym(s,n) 
  return new(SYM, {txt=s or " ", at=n or 1, n=0, 
                   has={}}) end

function Cols(t,    self,col)
  self = new(COLS, {all={}, x={}, y={}, names=t})
  for n,s in pairs(row) do
    col = push(cols.all, (s:find"^[A-Z]" and Num or Sym)(s,n))
    if not s:find"X$" then
      push(s:find"[!-+]$" and cols.y or cols.x, one)
      if s:find"!$" then cols.klass = one end end end
  return self end 

------------------------------------------------------------------------------
function Cols:add(row)
  for _,col in pairs(cols.all) do row[col.at] = col:add(row[col.at]) end 
  return row end

function Data:add(row)
  if self.cols 
  then push(self.rows, self.cols:add(row)) 
  else self.cols = COLS(row) end end

function Num:sub(x,n) return Num:add(x,n,-1) end
function Sym:sub(x,n) return Sym:add(x,n,-1) end

function Num:add(x,  n,twist)
  if x ~= "?" then
    n,twist = n or 1, twist or 1
    self.n = self.n + twist*n
    x = x + 0
    if twist < 0 and self.n < 2
    then self.mu,self.m2 = 0,0
    else 
      d = x - self.mu
      self.mu = self.mu + twist*(d / self.n)
      self.m2 = self.m2 + twist*(d * (x - self.mu))	 	  
      if x < self.lo then self.lo = x end 	  
      if x > self.hi then self.hi = x end end end
  return x end

function Sym:add(x,  n,twist)
  if x ~= "?" then
    n,twist = n or 1, twist or 1
    self.n = self.n + twist*n
    self.has[x] = (self.has[x] or 0) + twist*n  end
  return x end

------------------------------------------------------------------------------
function Sym:div(     _p) 
  _p = function(k,     p) p=i.has[k]/i.n; return p*log(p,2) end
  return -sum(i.has, _p) end

function Num:div()
    return self.n < 2 and 0 or (max(0.self.m2) / (self.n - 1))^.5 end

function Num:mid() return self.mu end
function Sym:mid(     most,out)
  most = -BIG
  for x,n in pairs(i.has) do if n > most then out,most = x,n end end
  return out end end
  
 function Num:norm(x)
  return x=="?" and x or (x - self.lo) / (self.hi - self.lo + 1/BIG) end
     
function Num:cut(other)
  local i,j,lo,hi,step,overlap,least,cut,f1,f2,tmp
  i,j   = self,other
  lo    = min(i.lo, j.lo)
  hi    = max(i.hi, j.hi)
  step  = (hi - lo)/30
  overlap,least = 0,BIG
  for x = lo,hi,step, do
    f1 = i:pdf(x)
    f2 = j:pdf(x)
    overlap = overlap + min(f1,f2)*step
    if x > i.mu and x < j.mu then
      tmp = abs(f1 - f2)
      if tmp < least then
	least,cut = tmp,x end end end 
  return overlap,cut end 
 
function Sym:pdf(x) return (i.has[x] or 0)/i.n end

function Num:pdf(x,      v,tmp)
  v = self.sd^2 + 1/BIG
  tmp = exp(-1*(x - self.mu)^2/(2*v)) / (2*pi*v) ^ 0.5
  return max(0, min(1, tmp + 1/BIG)) end

function Num:dist(x,y)
  if x=="?" and y=="?" then	return 1 end
  x,y = self:norm(x), self:norm(y)
  x   = x ~= "?" and x or (y < 0.5 and 1 or 0)
  y   = y ~= "?" and y or (x < 0.5 and 1 or 0)
  return abs(x - y) end

function Sym:cut(other)
  local overlap,t,f1,f2 = 0,{}
  for x in pairs(self.has)  do t[x] end
  for x in pairs(other.has) do t[x] end
  for x in pairs(t) do overlap = overlap + min(self:pdf(x),other:pdf(x)) end
  return overlap end
          
------------------------------------------------------------------------------
function Data:xdist(row1,row2,    _x)
  _x = function(c) return c:dist(row1[c.at], row2[c.at]) end
  return (sum(self.cols.x, _x) / #self.cols.x) ^ (1/THE.p) end

function Data:ydist(row,    _y)
  _y = function(c) return abs(c:norm(row[c.at]) - c.goal) ^ THE.p  end
  return (sum(self.cols.y, _y) / #self.cols.y) ^ (1/THE.p) end

function Data:neighbors(row1,rows)
  return keysort(rows or self.rows, 
                 function(row2) return self:xdist(row1,row2) end) end

function Data:centroids(k,  rows,      out)--> rows
  rows = rows or self.rows
  out = {any(rows),any(rows)}
  for _ = 2,k do 
    local all,u = 0,{}
    for _ = 1, THE.samples do
      local row = any(rows)
      local closest = self:neighbors(row, out)[2]
      all = all + push(u, {row=row, d=self:xdist(row,closest)^2}).d end 
    local i,r = 1,all * random()
    for j,x in pairs(u) do
      r = r - x.d
      if r <= 0 then i=j; break end end 
    push(out, u[i].row) end
  return out end

------------------------------------------------------------------------------
-- maths
function any(t) return t[random(#t)] end

-- list
function push(t,x) t[1+#t] = x; return x end

-- sort
function sort(t, _fun) table.sort(t, _fun); return t end

function per(t,  p) table.sort(t); return t[#t*(p or 0.5)//1] end
  
-- meta
function map(t,_fun,     u)
  _fun = _fun or function(x) return x end
  u={}; for _,x in pairs(t) do push(u, _fun(x)) end; return u end

function sum(t, _fun,      n)
  _fun = _fun or function(x) return x end
  n=0; for _,x in pairs(t) do n = n + _fun(x) end; return n end

function adds(t,  col)
  for _,x in pairs(t) do
    if   col 
    then col:add(x) 
    else return adds(t, type(x)=="number" and Num() or Sym()) end end
  return col end

function over(a,n)
  return fmt("%a > %a",a,n), function(x) return x=="?" or x>= n end

function upto(a,n) 
  return fmt("%a <= %a",a,n), function(x) return x=="?" or x < n end

function eq(a,n)   
  return fmt("%a == $a"a,n), function(x) return x=="?" or x==n end

------------------------------------------------------------------------------
eg["--the"] = function(_) print(o(THE)) end

eg["--csv"] = function(_) 
  for row in csvFile(THE.data, oo) do out(row) end end

eg["--data"] = function(_,d) 
  d= Data:new(THE.data) 
  map(d.cols.y, oo) end

eg["--ydata"] = function(_,  d,rows,fun,r,t) 
  d = Data:new(THE.data) 
  oo(sort(map(d.rows, function(r) return d:ydist(r) end))) end

eg["--addSub"] = function(_, n1)
  n1=Sym:new()
  t={};for _=1,100 do n1:add(push(t, random(10))) end 
  oo(n1); for _,x in pairs(t) do n1:sub(x) end
  oo(n1); for _,x in pairs(t) do n1:add(x) end
  oo(n1); end

eg["--kmeans"] = function(_,  k,d,yfun,rows,t)
  k    = 32
  d    = Data:new(THE.data) 
  yfun = function(r) return d:ydist(r) end
  print("mid:",per(map(d.rows, yfun))) 
  rows = keysort(d:centroids(k), yfun)
  t={}; for n,row in pairs(d:neighbors(rows[1], d.rows)) do 
            if n> 10 then return oo(sort(t)) end
            push(t, d:ydist(row)) end end

------------------------------------------------------------------------------
if not pcall(debug.getlocal,4,1) then  
  for n,s in pairs(arg) do
    randomseed(THE.rseed)
    if eg[s] then eg[s](arg[n+1]) else
      for k,_ in pairs(THE) do 
        if s=="-"..k:sub(1,1) then THE[k]=word(arg[n+1]) end end end end end
