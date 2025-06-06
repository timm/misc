--     _|   _.  _|_   _. 
--    (_|  (_|   |_  (_| 

local l = require"lib"
local the = require"about"
local Sym,Num,Cols,Data = {},{},{},{}

-------------------------------------------------------------------------------
function Sym:new(txt,pos)
  return l.new(Sym, {txt=txt or "", pos=pos or 0, n=0,
                   has={}, most=0, mode=nil}) end

function Sym:add(x,  n)
 if x=="?" then return x end
 n = n or 1
 self.n = self.n + n
 self.has[x] = n + (self.has[x] or 0)
 if self.has[x] > self.most then
   self.most, self.mode = self.has[x], x end end

function Sym:ent(       e)
  e=0; for _,n in pairs(i.has) do e = e - n/i.n * math.log(n/i.n, 2) end
  return e end

function Sym:merged(other,  tiny,      i,j,k)
  tiny = tiny or 0
  i,j,k = self, other, Sym:new(self.txt, self.pos)
  for _,has in pairs{i.has, j.has} do
    for x,n in pairs(has) do
      k:add(x,n) end end
  if i.n < tiny or j.n < tiny or k:ent() <= (i.n*i:ent() + j.n*j:ent()) / k.n 
  then return k end end

-------------------------------------------------------------------------------
function Num:new(txt,pos)
  return l.new(Num, {txt=txt or "",pos=pos or 0, n=0,
                   mu=0, m2=0, sd=0, lo=the.big, hi= -the.big,
                   goal = (txt or ""):find"-$" and 0 or 1}) end

function Num:add(n)
  if n=="?" then return n end
  self.n  = self.n + 1
  local d = n - self.mu
  self.mu = self.mu + d/self.n
  self.m2 = self.m2 + d*(n - self.mu)
  self.sd = self.n < 2 and 0 or (self.m2/(self.n - 1))^0.5
  self.lo = math.min(n, self.lo)
  self.hi = math.max(n, self.hi) end

function Num:norm(x)
  return x=="?" and x or (x - self.lo) / (self.hi - self.lo + 1/the.big) end

--------------------------------------------------------------------------------
function Cols:new(names)
  local all,x,y,klass = {}, {}, {}, nil
  for n,s in pairs(names) do
    local col = l.push(all, (s:find"^[A-Z]" and Num or Sym):new(s,n) )
    if not s:find"X$" then
      l.push(s:find"[!+-]$" and y or x, col)
      if s:find"!$" then klass= col end end end
  return l.new(Cols, {all=all, x=x, y=y, names=names, klass=klass}) end 

function Cols:add(row)
  for _,col in pairs(self.all) do col:add(row[col.pos]) end 
  return row end

--------------------------------------------------------------------------------
function Data:new(src)
  self = l.new(Data, {rows={}, cols=nil})
  if type(src)=="string" 
  then for   row in l.csv(src)   do self:add(row) end
  else for _,row in pairs(src) do self:add(row) end end
  return self end

function Data:add(row)
  if   self.cols 
  then l.push(self.rows, self.cols:add(row))
  else self.cols = Cols:new(row) end
  return self end

return {Num=Num, Sym=Sym, Data=Data}
