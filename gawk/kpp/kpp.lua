local any,cells,coerce,csv,fmt,map,min,new,o,push
local Big, Num, Sym, Cols, Data = 1E32, {}, {}, {}, {}
local the = {file = "../../../moot/optimize/misc/auto93.csv",
             samples=32,
             k = 25,
             seed = 1234567891}

function new(mt,a) mt.__index = mt; return setmetatable(a,mt) end

-------------------------------------------------------------------------------  
function Num:new(txt,pos)
  return new(Num, {txt=txt or "",pos=pos or 0, n=0,
                   mu=0, m2=0, sd=0, lo=Big, hi= -Big,
                   goal = (txt or ""):find"-$" and 0 or 1}) end

function Sym:new(txt,pos)
  return new(Sym, {txt=txt or "", pos=pos or 0, n=0,
                   has={}, most=0, mode=nil}) end

function Cols:new(names)
  local all,x,y,klass = {}, {}, {}, nil
  for n,s in pairs(names) do
    local col = push(all, (s:find"^[A-Z]" and Num or Sym):new(s,n) )
    if not s:find"X$" then
      push(s:find"[!+-]$" and y or x, col)
      if s:find"!$" then klass= col end end end
  return new(Cols, {all=all, x=x, y=y, names=names, klass=klass}) end 

function Data:new(src)
  self = new(Data, {rows={}, cols=nil})
  if type(src)=="string" 
  then for   row in csv(src)   do self:add(row) end
  else for _,row in pairs(src) do self:add(row) end end
  return self end

-------------------------------------------------------------------------------  
function Sym:add(x)
  if x=="?" then return x end
  self.n = self.n + 1
  self.has[x] = 1 + (self.has[x] or 0)
  if self.has[x] > self.most then
    self.most, self.mode = self.has[x], x end end

function Num:add(n)
  if n=="?" then return n end
  self.n  = self.n + 1
  local d = n - self.mu
  self.mu = self.mu + d/self.n
  self.m2 = self.m2 + d*(n - self.mu)
  self.sd = self.n < 2 and 0 or (self.m2/(self.n - 1))^0.5
  self.lo = math.min(n, self.lo)
  self.hi = math.max(n, self.hi) end

function Cols:add(row)
  for _,col in pairs(self.all) do col:add(row[col.pos]) end 
  return row end

function Data:add(row)
  if   self.cols 
  then push(self.rows, self.cols:add(row))
  else self.cols = Cols:new(row) end
  return self end

-------------------------------------------------------------------------------  
function Data:xdist(row1,row2,      d)
  d = 0
  for _,col in pairs(self.cols.x) do
    d = d + col:dist(row1[col.pos], row2[col.pos])^2 end
  return (d / #self.cols.x) ^ 0.5 end

function Data:ydist(row,      d)
  d = 0
  for _,col in pairs(self.cols.y) do
    d = d + col:dist(col:norm(row[col.pos]) - col.goal)^2 end
  return (d / #self.cols.y) ^ 0.5 end

function Num:norm(x)
  return x=="?" and x or (x - self.lo) / (self.hi - self.lo + 1/Big) end

function Data:around(budget,  rows,      z)
  rows = rows or self.rows
  z = {any(rows)}
  for _ = 2,budget do 
    local all,u = 0,{}
    for _ = 1,math.min(the.samples, #rows) do
      local row = any(rows)
      local closest = min(z, function(maybe) return self:xdist(row,maybe) end) 
      all = all + push(u,{row=row, d=self:xdist(row,closest)^2}).d end 
    local r = all * math.random()
    local one
    for _,x in pairs(u) do
      one = x
      r = r - x.d
      if r <= 0 then break end end 
    push(z, one) end
  return z end

-------------------------------------------------------------------------------  
function any(t) return t[math.random(#t)] end

function cells(s,  z) 
  z={}; for s2 in s:gmatch"([^,]+)" do z[1+#z]=coerce(s2) end; return z end

function coerce(s) return tonumber(s) or s:match"^%s*(.-)%s*$" end

function csv(src)
  src = io.input(src)
  return function(      s)
    s = io.read(); if s then return cells(s) else io.close(src) end end end

fmt=string.format

function map(t,f,    z)
  z={}; for _,x in pairs(t) do z[1+#z] = f(x) end; return z end

function min(t,f,    lo,n,z)
  lo = Big
  for _,x in pairs(t) do 
    z= z or x
    n=f(x); if n < lo then lo,z = n,x end end
  return z end

function o(x,          t,list,dict)
  t = {}
  list= function() for k,v in pairs(x) do t[1+#t]= o(v) end end
  dict= function() for k,v in pairs(x) do t[1+#t]= fmt(":%s %s",k,o(v)) end end
  if type(x) == "number" then return fmt(x//1 == x and "%s" or "%.3g",x) end
  if type(x) ~= "table"  then return tostring(x) end
  if #x>0 then list() else dict(); table.sort(t) end
  return "{" .. table.concat(t, " ") .. "}" end

function push(t,x) t[1+#t]=x; return x end

-------------------------------------------------------------------------------  
local go={}

go["--data"] = function(file) 
                 print(o(Data:new(file or the.file).cols.y[1])) end

go["--around"] = function(file) 
                   print(Data:new(file or the.file):around(the.k)) end

for k,s in pairs(arg) do
  math.randomseed(the.seed)
  if go[s] then go[s]( arg[k+1] ) end end

