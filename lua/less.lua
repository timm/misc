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

local BIG = 1E32
local eg, Data = {},{}

------------------------------------------------------------------------------
local csv,fmt,olist,dlist,o,oo	  	 	  	 	  

function csv(src,_fun,     s,t,src)
  src = io.input(src)
  s = io.read()
  while s do 
    t={}; for s1 in s:gmatch"([^,]+)" do t[1+#t]=s1 end
    _fun(t) 
    s = io.read() end
  io.close(src) end

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

------------------------------------------------------------------------------
function Data:new()
   return new(Data, {n={}, txt={}, x={}, y={}, has={},
                     mu={}, m2={}, hi={}, lo={}} ) end

function Data.cols(i,row)
  i.txt = row
  for c,s in pairs(row) do
    i[s:find"[!+-]$" and "y" or "x"][c] = s:find"-$" and 0 or 1 
    if   s:find"^[A-Z]" 
    then i.mu[c], i.m2[c], i.lo[c], i.hi[c] = 0, 0, BIG, -BIG 
    else i.has[c] = {} end end
  return row end 
  
function Data.add(i,row,     x,d,_sym,_num)
  _sym = function(c,x) 
     i.has[c][x] = (i.has[c][x] or 0) + 1 end
  _num = function(c,x,     d)
     row[c]  = x + 0
     d       = x - i.mu[c]
     i.mu[c] = i.mu[c] + d / i.n 
	   i.m2[c] = i.m2[c] + d * (x - i.mu[c])	 	  
	 	 if x < i.lo[c] then i.lo[c] = x end 	  
	 	 if x > i.hi[c] then i.hi[c] = x end end
	 	 
  i.n[c] = i.n[c] + 1
  for c,x in pairs(row) do 
    if x ~= "?" then (x.mu[c] and _num or _sym)(c,x) end end
	return row end	 

function Data.sub(i,row,     x,d,_sym,_num)
  _sym = function(c,x) 
     i.has[c][x] = i.has[c][x] - 1 end
  _num = function(c,x,     d)
     if   i.n[c] < 2
     then i.mu[c], i.m2[c] = 0,0
     else d = x - i.mu[c]
          i.mu[c] = i.mu[c] - d / i.n
          i.m2[c] = i.m2[c] - d * (x - i.mu[c]) end end
	 	 
  i.n[c] = i.n[c] - 1
  for c,x in pairs(row) do 
    if x ~= "?" then (x.mu[c] and _num or _sym)(c,x) end end
	return row end	 	  	 	  	 	  

function Data.norm(i,c,x)
  return x=="?" and x or (x - i.lo[c]) / (i.hi[c] - i.lo[c] + 1/BIG) end
  
function Data.mid(i,c,     most,out)
  if i.mu[c] then return i.mu[c] else 
    most = -BIG
    for x1,n in pairs(i.has[c]) do if n > most then out,most = x1,n end end
    return out end end
       
function Data.div(i,c) 
  if   i.mu[c] 
  then return i.n[c]<2 and 0 or (max(0,i.m2[c]) / (i.n[c] - 1))^.5 
  else e = 0
       for _,n in pairs(i.has[c]) do e = e - n/i.n[c] * log(n/i.n[c],2) end
       return e end end

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
 
function Num:pdf(x,      v,tmp)
  v = self.sd^2 + 1/BIG
  tmp = exp(-1*(x - self.mu)^2/(2*v)) / (2*pi*v) ^ 0.5
  return max(0, min(1, tmp + 1/BIG)) end

function Num:dist(x,y)
  if x=="?" and y=="?" then	
  n return 1 end
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
          
function Sym:pdf(x) return (i.has[x] or 0)/i.n end

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
pi  = math.pi
abs = math.abs
exp = math.exp
log = math.log
max = math.max
min = math.min
random = math.random
randomseed = math.randomseed

function any(t) return t[random(#t)] end

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
