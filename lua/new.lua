local b4={}; for k, _ in pairs(_ENV) do b4[k]=k end

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
local eg,Data,Num,Sym,Cols = {},{},{},{},{}
------------------------------------------------------------------------------
local fmt,any,num,push,new,lt,sort,keysort,map,per,sum
local olist,odict,o,oo,word,words,csv,adds,minkowski

-- polymorphism
function new(isa,i) isa.__index=isa; setmetatable(i,isa); return i end

-- maths
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
  
-- m
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

function minkowski(cols, p, _fun,  _do)
  _di = function(col) return _fun(col) ^ p end
  return (sum(cols, _do) / #cols) ^ (1/p) end

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
function Num:new(txt,at)
   return new(Num,{txt=txt or " ", at=at or 0, n=0, hi= -BIG, lo= BIG, 
                   goal = tostring(txt):find"-$" and 0 or 1}) end  

function Num:add(x,     d)
  if x ~= "?" then
    self.n = self.n + 1
    if x > self.hi then self.hi = x end
    if x < self.lo then self.lo = x end end end

function Num:norm(x)
  return x=="?" and x or (x - self.lo) / (self.hi - self.lo) end

function Num:dist(x,y)
  if x=="?" and y=="?" then return 1 end
  x,y = self:norm(x), self:norm(y)
  x   = x ~= "?" and x or (y < 0.5 and 1 or 0)
  y   = y ~= "?" and y or (x < 0.5 and 1 or 0)
  return math.abs(x - y) end

------------------------------------------------------------------------------
function Sym:new(txt, at)
   return new(Sym, {txt=txt or "", at=at or 0, n=0 }) end

function Sym:add(x)
  if x ~= "?" then
    self.n = self.n + 1 end end
 
function Sym:dist(x,y)
  return x=="?" and y=="?" and 1 or x==y and 0 or 1 end

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
  _y = function(c) return math.abs(c:norm(row[c.at]) - c.goal) ^ the.p  end
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
    local i,r = 1,all * math.random()
    for j,x in pairs(u) do
      r = r - x.d
      if r <= 0 then i=j; break end end 
    push(out, u[i].row) end
  return out end
       
------------------------------------------------------------------------------
eg["--the"]  = function(_) print(o(the)) end

eg["--csv"]  = function(_) csv(the.csv, oo) end

eg["--data"] = function(_,d) 
  d= Data:new(the.csv) 
  map(d.cols.y, oo) end

eg["--ydata"] = function(_,  d,rows,fun,r) 
  d = Data:new(the.csv) 
  oo(sort(map(d.rows, function(r) return d:ydist(r) end))) end

eg["--kmeans"] = function(_,  d,rows,fun,tmp) 
  d = Data:new(the.csv) ; print(">>")
  fun = function(r) return d:ydist(r) end
  print("mid:",per(map(d.rows, fun)))
  rows = d:centroids(16)
  tmp={}; for _=1,16 do push(tmp,d:ydist(any(rows))) end
  print(oo(sort(tmp)))
  for _,row in pairs(keysort(rows, fun)) do
    print(d:ydist(row)) end end

------------------------------------------------------------------------------
if not pcall(debug.getlocal,4,1) then  
  for n,s in pairs(arg) do
    math.randomseed(the.rseed)
    if eg[s] then eg[s](arg[n+1]) else
      for k,_ in pairs(the) do 
        if s=="-"..k:sub(1,1) then the[k]=word(arg[n+1]) end end end end end

for k,v in pairs(_ENV) do if not b4[k] then print("?",k,type(k)) end end 
