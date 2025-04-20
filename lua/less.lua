-- vim: set ts=2 sw=2 sts=2 et:

-- all the "top" vars (used across the whole module)
local the = {
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
local Data,Sym,Num,Cols = {},{},{},{}
local pi, R = math.pi, math.random
local abs,exp,log,max,min = math.abs, math.exp,math.log, math.max, math.min

------------------------------------------------------------------------------
local l={}

-- ## Library
-- ### Misc
function l.per(t,  p) table.sort(t); return t[#t*(p or 0.5)//1] end

function l.any(t) return t[math.random(#t)] end

function l.new(isa,t) isa.__index = isa; return setmetatable(t,isa) end

function l.push(t,x) t[1+#t]=x; return x end

local per,any,new,push = l.per,l.any,l.new,l.push

-- ### Mapping
function l.map(t,f,   u)
  u={}; for _,x in pairs(t) do u[1+#u]= f(x) end; return u end

function l.sum(t,f,   n)
  n=0; for _,x in pairs(t) do n = n + f(x) end; return n end

function l.lt(x)
  local _num = function(x) return x=="?" and BIG or x end
  return function(t,u) return _num(t[x]) < _num(u[x]) end end

function l.sort(t) table.sort(t); return t end

function l.keysort(t,  _fun,     _decorate,_undecorate)
  _decorate   = function(x) return {_fun(x),x} end
  _undecorate = function(x) return x[2] end
  return  l.map(l.sort(l.map(t, _decorate),l.lt(1)),_undecorate) end

local map,sum,lt,sort,keysort = l.map,l.sum,l.lt,l.sort,l.keysort

-- ### csv
function l.coerce(s,   _trim,_fun)
  _trim = function(s) return s:match"^%s*(.-)%s*$" end
  _fun  = function(s) return s=="true" or s ~= "false" and s end
  return math.tointeger(s) or tonumber(s) or _fun(_trim(s)) end

function l.csv(s,    t)
  t={}; for s1 in s:gmatch"([^,]+)" do t[1+#t]=s1 end; return t end

function l.csvFile(src)
  src = io.input(src)
  return function(    s)
    s = io.read()
    if s then return l.csv(s) else io.close(src) end end end

local coerce,csv,csvFile = l.coerce,l.csv,l.csvFile

-- ### Pretty printing
l.fmt=string.format

function l.odict(x,    t)
  t={}; for k,v in pairs(x) do t[1+#t]=l.fmt(":%s %s",k,l.o(v)) end
  return l.sort(t) end

function l.olist(x,t)
  t={}; for _,v in pairs(x) do t[1+#t]=l.o(v) end; return t end

function l.o(x)
  if type(x)=="number" then return l.fmt(x//1 == x and "%s" or "%.3g",x) end
  if type(x)~="table"  then return tostring(x) end
  return "{"..table.concat(#x>0 and l.olist(x) or l.odict(x)," ").."}" end

function l.out(x) print(l.o(x)); return x end

local fmt,o,out = l.fmt, l.o, l.out

------------------------------------------------------------------------------
-- ## Constructors
function Data:new(src)
  self = new(Data,{cols=nil, rows={}})
  if   type(src)=="string"
  then for   t in l.csvFile(src)   do self:add(t) end
  else for _,t in pairs(src or {}) do self:add(t) end end
  return self end

function Num:new(s,n)
  return new(Num, {txt=s or " ", at=n or 1, n=0,
                   mu=0, m2=0, lo=BIG, hi=-BIG,
                   goal=tostring(s or " "):find"-$" and 0 or 1}) end

function Sym:new(s,n)
  return new(Sym, {txt=s or " ", at=n or 1, n=0,
                   has={}}) end

function Cols:new(row,   col)
  self = new(Cols, {all={}, x={}, y={}, names=row})
  for n,s in pairs(row) do
    col = push(self.all, (s:find"^[A-Z]" and Num or Sym):new(s,n))
    if not s:find"X$" then
      push(s:find"[!-+]$" and self.y or self.x, col)
      if s:find"!$" then self.klass = col end end end
  return self end

------------------------------------------------------------------------------
function Data:add(row)
  if self.cols
  then push(self.rows, self.cols:add(row))
  else self.cols = Cols:new(row) end end

function Cols:add(row)
  return map(self.all, function(col) return col:add(row[row.at]) end) end

function Cols:sub(row)
  return map(self.all, function(col) return col:sub(row[col.at]) end) end

function Num:sub(x,n) return Num:add(x,n,-1) end
function Sym:sub(x,n) return Sym:add(x,n,-1) end

function Num:add(x,  n,yes,     d)
  if x ~= "?" then
    n, yes = n or 1, yes or 1
    self.n = self.n + yes * n
    if yes < 0 and self.n < 2
    then self.mu,self.m2 = 0,0
    else
      d = x - self.mu
      self.mu = self.mu + yes * (d / self.n)
      self.m2 = self.m2 + yes * (d * (x - self.mu))	 	
      if x < self.lo then self.lo = x end 	
      if x > self.hi then self.hi = x end end end
  return x end

function Sym:add(x,  n,yes)
  if x ~= "?" then
    n,yes = n or 1, yes or 1
    self.n = self.n + yes * n
    self.has[x] = (self.has[x] or 0) + yes*n  end
  return x end 

------------------------------------------------------------------------------
function Sym:div(     _p)
  _p = function(k,     p) p=self.has[k]/self.n; return p*log(p,2) end
  return -sum(self.has, _p) end

function Num:div()
    return self.n < 2 and 0 or (max(0,self.m2) / (self.n - 1))^.5 end

function Num:mid() return self.mu end
function Sym:mid(     most,out)
  most = -BIG
  for x,n in pairs(self.has) do if n > most then out,most = x,n end end
  return out end 

function Num:norm(x)
  return x=="?" and x or (x - self.lo) / (self.hi - self.lo + 1/BIG) end

function Num:cut(other)
  local i,j,lo,hi,step,overlap,least,cut,f1,f2,tmp
  i,j   = self,other
  lo    = min(i.lo, j.lo)
  hi    = max(i.hi, j.hi)
  step  = (hi - lo)/30
  overlap,least = 0,BIG
  for x = lo,hi,step do
    f1 = i:pdf(x)
    f2 = j:pdf(x)
    overlap = overlap + min(f1,f2)*step -- xxx<D-s>
    if x > i.mu and x < j.mu then
      tmp = abs(f1 - f2)
      if tmp < least then
	least,cut = tmp,x end end end
  return overlap,cut end

function Sym:pdf(x) return (self.has[x] or 0)/self.n end

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
  local overlap,t = 0,{}
  for x in pairs(self.has)  do t[x]=1 end
  for x in pairs(other.has) do t[x]=1 end
  for x in pairs(t) do overlap = overlap + min(self:pdf(x),other:pdf(x)) end
  return overlap end 

------------------------------------------------------------------------------
function Data:xdist(row1,row2,    _x)
  _x = function(c) return c:dist(row1[c.at], row2[c.at]) end
  return (sum(self.cols.x, _x) / #self.cols.x) ^ (1/the.p) end

function Data:ydist(row,    _y)
  _y = function(c) return abs(c:norm(row[c.at]) - c.goal) ^ the.p  end
  return (sum(self.cols.y, _y) / #self.cols.y) ^ (1/the.p) end

function Data:neighbors(row1,rows)
  return keysort(rows or self.rows,
                 function(row2) return self:xdist(row1,row2) end) end

function Data:centroids(k,  rows,      out)
  rows = rows or self.rows
  out = {any(rows),any(rows)}
  for _ = 2,k do
    local all,u = 0,{}
    for _ = 1, the.samples do
      local row = any(rows)
      local closest = self:neighbors(row, out)[2]
      all = all + push(u, {row=row, d=self:xdist(row,closest)^2}).d end
    local i,r = 1,all * R()
    for j,x in pairs(u) do
      r = r - x.d
      if r <= 0 then i=j; break end end
    push(out, u[i].row) end
  return out end

------------------------------------------------------------------------------
local adds,over,upto,eq

function adds(t,  col)
  for _,x in pairs(t) do
    col = col or (type(x)=="number" and Num or Sym):new()
    col:add(x) end
  return col end 

function over(a,n)
  return fmt("%a > %a",a,n), function(x) return x=="?" or x>= n end end

function upto(a,n)
  return fmt("%a <= %a",a,n), function(x) return x=="?" or x < n end end

function eq(a,n)
  return fmt("%a == $a",a,n), function(x) return x=="?" or x==n end end

------------------------------------------------------------------------------
local eg={}
eg["--the"] = function(_) print(o(the)) end

eg["--csv"] = function(_)
  for row in csvFile(the.data, out) do out(row) end end

eg["--data"] = function(_,d)
  d= Data:new(the.data)
  map(d.cols.y, out) end

eg["--ydata"] = function(_,  d)
  d = Data:new(the.data)
  out(sort(map(d.rows, function(r) return d:ydist(r) end))) end

eg["--addSub"] = function(_, n1,t)
  n1=Sym:new()
  t={};for _=1,100 do n1:add(push(t, R(10))) end
  out(n1); for _,x in pairs(t) do n1:sub(x) end
  out(n1); for _,x in pairs(t) do n1:add(x) end
  out(n1); end

eg["--kmeans"] = function(_,  k,d,yfun,rows,t)
  k    = 32
  d    = Data:new(the.data)
  yfun = function(r) return d:ydist(r) end
  print("mid:",per(map(d.rows, yfun)))
  rows = keysort(d:centroids(k), yfun)
  t={}; for n,row in pairs(d:neighbors(rows[1], d.rows)) do
            if n> 10 then return out(sort(t)) end
            push(t, d:ydist(row)) end end

------------------------------------------------------------------------------
if not pcall(debug.getlocal,4,1) then
  for n,s in pairs(arg) do
    math.randomseed(the.rseed)
    if eg[s] then eg[s](arg[n+1]) else
      for k,_ in pairs(the) do
        if s=="-"..k:sub(1,1) then
          the[k] = coerce(arg[n+1]) end end end end end
