-- vim: set tabstop=2 shiftwidth=2 expandtab :

local b4={}; for k,_ in pairs(_ENV) do b4[k]=k end

local l,eg,the = {},{},{k=1,m=2,big=1E32}
local sum,coerce,csv,o,new 
local exp,log,min,max,pi = math.exp, math.log, math.min, math.max, math.pi

-------------------------------------------------------------------------------
local Num,Sym,Cols,Data = {},{},{},{}

function Num:new(txt,at) 
  return new(Num, {at=at or 0, txt=txt or "", n=0, 
                   mu=0, m2=0, sd=0, lo=the.big, hi= -the.big,
                   goal = (txt or ""):find"-$" and 0 or 1}) end

function Sym:new(txt,at) 
  return new(Sym, {at=at or 0, txt=txt or "", n=0, has={},most=0,mode=nil}) end

function Cols:new(names,    all,x,y,klass,col)
  all,x,y = {},{},{}
  for at,s in pairs(names) do
    col = (s:find"^[A-Z]" and Num or Sym):new(s,at)
    all[1+#all] = col
    if s:find"!$" then klass = col end
    if s:find"[!+-]$" then y[1+#y] = col else x[1+#x] = col end end
  return new(Cols, {all=all, y=y, x=x, klass=klass}) end

function Data:new() 
  return new(Data, {rows={}, cols=nil}) end

function eg.cols(_,    names)
  names={"cylinders", "Displacement", "HorsepowerX", 
         "Weight-", "Acceleration+", "Model", "origin", "Mpg+"}
  for _,col in pairs(Cols:new(names).all) do print(o(col)) end end

function eg.data(f)
  for _,col in pairs(Data:new():load(f).cols.all) do print(o(col)) end end

-------------------------------------------------------------------------------
function Sym:add(x)
  self.n = self.n + 1
  self.has[x] = 1 + (self.has[x] or 0)
  if self.has[x] > self.most then self.most, self.mode=self.has[x], x end end 

function Num:add(x,    d)
  self.lo = min(x, self.lo)
  self.hi = max(x, self.hi)
  self.n  = self.n + 1
  d = x - self.mu
  self.mu = self.mu + d/self.n
  self.m2 = self.m2 + d*(x - self.mu)
  self.sd = self.n < 2 and 0 or (self.m2 / (self.n - 1))^0.5 end 

function Data:load(src)
  if type(src) == "string" 
  then for   row in l.csv(src)       do self:add(row) end 
  else for _,row in pairs(src or {}) do self:add(row) end end
  return self end
    
function Data:add(row)
  if   self.cols 
  then self.rows[1+#self.rows] = self.cols:add(row)
  else self.cols = Cols:new(row) end end

function Cols:add(row)
  for at,x in pairs(row) do
    if x ~="?" then
      self.all[at]:add(x) end end
  return row end 

-------------------------------------------------------------------------------
function Sym:like(x,prior,    v,tmp) 
  if x~="?" then
    return ((self.has[x] or 0) + the.m*prior) / (self.n + the.m) end end

function Num:like(x,_,     v,tmp)
  if x~="?" then
    v = self.sd^2 + 1/the.big
    tmp = exp(-1*(x - self.mu)^2/(2*v)) / (2*pi*v) ^ 0.5
    return max(0,min(1, tmp + 1/the.big)) end end

function Data:like(row,  nall,nh,    prior,LIKE,LOG) 
  prior = (#self.rows + the.k) / (nall + the.k*nh)
  LIKE  = function(col) return LOG( col:like(row[col.at], prior) ) end
  LOG   = function(x)   return x and x>0 and log(x) or 0 end
  return LOG(prior) + sum(self.cols.x, LIKE) end

function eg.like(f,    d)
  d=Data:new():load(f)
  for _,row in pairs(d.rows) do print(d:like(row,1000,2)) end end

-------------------------------------------------------------------------------
function l.sum(t,f,   n)
  n=0; for _,x in pairs(t) do n=n+f(x) end; return n end

function l.coerce(s,     FUN,TRIM) 
  TRIM = function(s) return s:match"^%s*(.-)%s*$" end
  FUN  = function(s) return s=="true" and true or s ~= "false" and s end
  return math.tointeger(s) or tonumber(s) or FUN(TRIM(s)) end

function l.csv(file,     src) 
  if file and file ~="-" then src=io.input(file) end
  return function(     s,t)
    s = io.read()
    if s 
    then t={}; for s1 in s:gmatch"([^,]+)" do t[1+#t]=l.coerce(s1) end; return t 
    else if src then io.close(src) end end end end

function l.o(x,        t,FMT,NUM,LIST,DICT) 
  t = {}
  FMT  = string.format
  NUM  = function() return x//1 == x and tostring(x) or FMT("%.3g",x) end
  LIST = function() for k,v in pairs(x) do t[1+#t] = l.o(v) end end
  DICT = function() for k,v in pairs(x) do t[1+#t] = FMT(":%s %s",k, l.o(v)) end end
  if type(x) == "number" then return NUM() end 
  if type(x) ~= "table"  then return tostring(x) end
  if #x>0 then LIST() else DICT(); table.sort(t) end
  return "{" .. table.concat(t, " ") .. "}" end

function l.new(kl,t)
  kl.__index=kl; kl.__tostring = l.o; return setmetatable(t,kl) end

sum,coerce,csv,o,new = l.sum,l.coerce,l.csv,l.o,l.new

function eg.the(_) print(o(the)) end
function eg.csv(f) for row in csv(f) do print(o(row)) end end

-------------------------------------------------------------------------------
if arg[0] == "nb.lua" then 
  for j,s in pairs(arg) do
    s=s:gsub("[-][-]","")
    if eg[s] then  eg[s](arg[j+1]) end end 
  for k,v in pairs(_ENV) do if not b4[k] then print("?", k, type(v)) end end end

return {Num=Num, Sym=Sym, Data=Data, the=the, lib=l}
