local the = {
  about   = {what = "new.lua", 
             why  = "simple inference", 
             when = "(c) 2025, MIT License", 
             who  = "Tim Menzies"}, 
  rseed   = 1234567891,
  csv     = "../data/auto93.csv", 
  k       = 1,                   
  m       = 2,                  
  p       = 2,                 
  samples = 256               
}

local BIG=1E32
local eg, Data, Num, Sym, Cols = {},{},{},{},{}

local abs, adds, eq, fmt, any, log, lt, max, num, new, 
local o, oo, olist, odict, per, push, random, randomseed
local sort, sum, words, word, keysort, map, over, under, csv

------------------------------------------------------------------------------
function Num:new(txt,at)
   return new(Num,{txt=txt or " ", at=at or 0, n=0,
                   hi= -BIG, lo= BIG, mu=0, m2=0,
                   goal = tostring(txt):find"-$" and 0 or 1}) end

function Num:add(x)
  if x ~= "?" then
    self.n  = self.n + 1
    local d = x - self.mu
    self.mu = self.mu + d / self.n
    self.m2 = self.m2 + d * (x - self.mu)
    if x > self.hi then self.hi = x end
    if x < self.lo then self.lo = x end end
  return x end

function Num:sub(x)
  if x ~= "?" then
    self.n = self.n - 1
    if   self.n < 2
    then self.mu, self.m2 = 0,0
    else local d = x - self.mu
         self.mu = self.mu - d / self.n
         self.m2 = self.m2 - d * (x - self.mu) end end
  return x end

function Num:mid() return self.mu end

function Num:div() 
  return self.n < 2 and 0 or (max(0,self.m2)/(self.n - 1))^.5 end

function Num:norm(x)
  return x=="?" and x or (x - self.lo) / (self.hi - self.lo) end

function Num:dist(x,y)
  if x=="?" and y=="?" then return 1 end
  x,y = self:norm(x), self:norm(y)
  x   = x ~= "?" and x or (y < 0.5 and 1 or 0)
  y   = y ~= "?" and y or (x < 0.5 and 1 or 0)
  return abs(x - y) end

function Num:cuts(rows, Y,Klass,epsN,epsY)
  local _xy, lhs,rhs,least,cut,tmp
  _xy = function(row) return {x=row[self.at], y=rhs:add(Y(row))} end
  lhs, rhs = Num(), Klass()
  xys = map(sort(rows, lt(self.at)), _xy)
  least,cut = BIG,nil
  for n,xy in pairs(xys) do
    if xy.x == "?" then break end
    lhs:add( rhs:sub(xy.y) )
    if n > epsN and n < #xys - epsN then
      if xy.x ~= xys[n+1].x then
        if abs(lhs.mu - rhs.mu) > epsY then
          tmp = (lhs.n * lhs:div() + rhs.n * rhs:div()) / self.n     
          if tmp < least then
             least, cut = tmp, xys[n+1].x end end end end end
  if cut then
    return least, [upto(self.txt, xy.x), over(self.txt, xy.x)] end end
 
------------------------------------------------------------------------------
function Sym:new(txt, at)
   return new(Sym, {txt=txt or "", at=at or 0, n=0, has={}}) end

function Sym:add(x)
  if x ~= "?" then
    self.n = self.n + 1 
    self.has[x] = (self.has[x] or 0) + 1 end
  return x end

function Sym:sub(x)
  if x ~= "?" then
    self.n = self.n - 1 
    self.has[x] = self.has[x] - 1 end 
  return x end

function Sym:dist(x,y)
  return x=="?" and y=="?" and 1 or x==y and 0 or 1 end

function Sym:mid()
  local most, mode = 0, nil
  for x,n in pairs(self.has) do if n>most then most,mode = n,x end end
  return mode end

function Sym:div(     _p)
  _p = function(x) return x<=0 and 0 or n/self.n * log(n/self.n,2) end
  return - sum(self.has, _p) end

function Sym:cuts(rows, Y,Klass,_,__)
  local tmp = {}
  for _,row in pairs(rows) do
    local x = row[self.at]
    tmp[x] = tmp[x] or Klass()
    if x ~= "?" then tmp[x]:add(Y(row)) end end 
  return (sum(tmp, function(x) return tmp[x].n*tmp[x]:div()/self.n end), 
          map(tmp, function(x) return eq(self.txt.x) end))

------------------------------------------------------------------------------
function Cols:new(names)
  local x,y,all,klass = {}, {}, {}, nil
  for n,s in pairs(names) do
    local col = push(all, (s:find"^[A-Z]" and Num or Sym):new(s,n))
    if not s:find"X$" then 
      push(s:find"[!+-]$" and y or x, col)
      if s:find"!$" then klass=col end end end
  return new(Cols, {x=x, y=y, all=all, klass=klass, names=names}) end 

function Cols:add(row)
  for _,col in pairs(self.all) do col:add(row[col.at]) end
  return row end

------------------------------------------------------------------------------
function Data:new(src,    _add)
  self = new(Data, {rows={}, cols=nil})
  _add = function(r) self:add(r) end
  if type(src)=="string" then csv(src, _add) else map(src,_add) end
  return self end

function Data:add(row)
  if   self.cols
  then push(self.rows, self.cols:add(row)) 
  else self.cols = Cols:new(row) end end

function Data:xdist(row1,row2,    _x)
  _x = function(c) return c:dist(row1[c.at], row2[c.at]) end
  return (sum(self.cols.x, _x) / #self.cols.x) ^ (1/the.p) end

function Data:ydist(row,    _y)
  _y = function(c) return abs(c:norm(row[c.at]) - c.goal) ^ the.p  end
  return (sum(self.cols.y, _y) / #self.cols.y) ^ (1/the.p) end

function Data:neighbors(row1,rows)
  return keysort(rows or self.rows, 
                 function(row2) return self:xdist(row1,row2) end) end

function Data:centroids(k,  rows,      out)--> rows
  rows = rows or self.rows
  out = {any(rows),any(rows)}
  for _ = 2,k do 
    local all,u = 0,{}
    for _ = 1, the.samples do
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
-- polymorphism
function new(isa,i) isa.__index=isa; setmetatable(i,isa); return i end

-- maths
abs = math.abs
log = math.log
max = math.max
random = math.random
randomseed = math.randomseed

function any(t) return t[math.random(#t)] end

-- list
function push(t,x) t[1+#t] = x; return x end

-- sort
function num(x) return x=="?" and BIG or x end

function lt(x) return function(t,u) return num(t[x]) < num(u[x]) end end

function sort(t, _fun) table.sort(t, _fun); return t end

function keysort(a,  _fun)
  local _decorate   = function(x) return {_fun(x),x} end
  local _undecorate = function(x) return x[2] end
  return  map(sort(map(a, _decorate), lt(1)), _undecorate) end

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

-- thing 2 strings
fmt=string.format

function olist(x,   t)
  t={}; for _,v in pairs(x) do t[1+#t]=o(v) end; return t end

function odict(x,   t)
  t={}; for k,v in pairs(x) do t[1+#t]=fmt(":%s %s",k,o(v)) end
  return sort(t) end

function o(x)
  return type(x) == "number" and fmt(x//1 == x and "%s" or "%.3g",x) or (
         type(x) ~= "table"  and tostring(x)                         or (
         "{".. table.concat(#x>0 and olist(x) or odict(x)," ") .."}" )) end 

function oo(x) print(o(x)); return x end

-- string to things
function word(s) return tonumber(s) or s:match("^%s*(.-)%s*$") end

function words(s,   t) 
  t={}; for s1 in s:gmatch"([^,]+)" do t[1+#t]=word(s1) end; return t end

function csv(src,_fun,     s)
  src = io.input(src)
  s = io.read()
  while s do _fun(words(s)); s=io.read() end
  io.close(src) end


------------------------------------------------------------------------------
eg["--the"]  = function(_) print(o(the)) end

eg["--csv"]  = function(_) csv(the.csv, oo) end

eg["--data"] = function(_,d) 
  d= Data:new(the.csv) 
  map(d.cols.y, oo) end

eg["--ydata"] = function(_,  d,rows,fun,r,t) 
  d = Data:new(the.csv) 
  oo(sort(map(d.rows, function(r) return d:ydist(r) end))) end

eg["--addSub"] = function(_, n1)
  n1=Sym:new()
  t={};for _=1,100 do n1:add(push(t, random(10))) end 
  oo(n1); for _,x in pairs(t) do n1:sub(x) end
  oo(n1); for _,x in pairs(t) do n1:add(x) end
  oo(n1); end

eg["--kmeans"] = function(_,  k,d,yfun,rows,t)
  k    = 32
  d    = Data:new(the.csv) 
  yfun = function(r) return d:ydist(r) end
  print("mid:",per(map(d.rows, yfun))) 
  rows = keysort(d:centroids(k), yfun)
  t={}; for n,row in pairs(d:neighbors(rows[1], d.rows)) do 
            if n> 10 then return oo(sort(t)) end
            push(t, d:ydist(row)) end end

------------------------------------------------------------------------------
if not pcall(debug.getlocal,4,1) then  
  for n,s in pairs(arg) do
    randomseed(the.rseed)
    if eg[s] then eg[s](arg[n+1]) else
      for k,_ in pairs(the) do 
        if s=="-"..k:sub(1,1) then the[k]=word(arg[n+1]) end end end end end
