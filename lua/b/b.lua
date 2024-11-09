local b4={}; for x,_ in pairs(_ENV) do b4[x]=x end
local the={train="../../data/auto93.csv",
           aq = {trainings = 1000,
                 start = 4,
                 stop  = 30},
           bayes  = {m = 2,
                     k = 1}}

local abs,exp,log     = math.abs,math.exp,math.log
local max,min,pi,sqrt = math.max,math.min,math.pi,math.sqrt
local adds, big, coerce, csv, kap, keysort,lt
local map, new, o, pront, push, sort, split,  sum

-- local function obj(s,      init,t,__)
--   t={a=s}; t.__index=t; return setmetatable(t,{__call=init(t)}) end
--
local Cols, Data, Num, Sym = {},{},{},{}

-------------------------------------------------------------------------------
function Sym:new(txt,at) 
  return new(Sym,{txt=txt or " ", at=at or 0, n=0, 
                  has={}, most=0, mode=nil}) end

function Sym:add(x)
  if x =="?" then return end
  self.n = self.n + 1
  self.has[x] = 1 + (self.has[x] or 0)
  if self.has[x] > self.most then 
    self.most, self.mode=self.has[x], x end end 

function Sym:like(x,prior)
  return  ((self.has[x] or 0) + the.bayes.m*prior) / (self.n + the.bayes.m) end

function Sym:mid() return self.mode end
-------------------------------------------------------------------------------
function Num:new(txt,at) 
  return new(Num, {txt=txt or " ", at=at or 0, n=0, 
                   mu=0, m2=0, sd=0, isNum=true, has={}, lo=big, hi= -big, 
                   goal=(txt or ""):find"-$" and 0 or 1}) end

function Num:add(x,     d)
  if x=="?" then return end
  self.n  = self.n + 1
  d       = x - self.mu
  self.mu = self.mu + d / self.n
  self.m2 = self.m2 + d * (x - self.mu)
  self.sd = self.n < 2 and 0 or (self.m2/(self.n - 1))^0.5
  if x > self.hi then self.hi = x end
  if x < self.lo then self.lo = x end end 

function Num:mid() return self.mu end

function Num:like(x,_,      v,tmp)
  v = self.sd^2 + 1/big
  tmp = exp(-1*(x - self.mu)^2/(2*v)) / (2*pi*v) ^ 0.5
  return max(0,min(1, tmp + 1/big)) end

function Num:norm(x)
  return x=="?" and x or (x - self.lo) / (self.hi - self.lo + 1/big) end

-------------------------------------------------------------------------------
function Cols:new(txts,     col)
  local all,x,y = {},{},{}
  for at,txt in pairs(txts) do
    col = (txt:find"^[A-Z]" and Num or Sym)(txt,at)
    push(all, col)
    if not txt:find"X$" then push(txt:find"[!+-]$" and y or x, col) end end
  return new(Cols,{txts=txts, all=all, x=x, y=y}) end

function Cols:add(row)
  map(self.all, function(col) col:add(row[col.at]) end)
  return row end

-------------------------------------------------------------------------------
function Data:new() 
  return new(Data,{cols=nil, rows={}}) end

function Data:clone(newRows)
  return Data:new():adds({self.cols.txt}):adds(newRows) end

function Data:read(file) 
  csv(file, function(t) self:add(t) end)
  return self end

function Data:adds(rows)
  map(rows or {}, function(t) self:add(t) end) 
  return self end

function Data:add(row) 
  if   self.cols 
  then push(self.rows, self.cols:add(row)) 
  else self.cols = Cols(row) end
  return self end

function Data:loglike(row, nall, nh,          prior,f,l)
  prior = (#self.rows + the.bayes.k) / (nall + the.bayes.k*nh)
  f     = function(x) return l( x:like(row[x.at],prior) ) end
  l     = function(n) return n>0 and log(n) or 0 end
  return l(prior) + sum(self.cols.x, f) end

function Data:ydist(row,     n,d)
  f = function(col) return (col:norm(row[col.at]) - col.goal)^2 end
  return (sum(self.cols.y,f) / #self.cols.y) ^ 0.5 end

function Data:acquire()
  local y,br,test,train,todo,done,best,rest,n,
  y  = function(r) return self:ydist(r) end
  br = function(r) return best:loglike(r,#done,2) - rest:loglike(r,#done,2) end
  train,test = split(shuffle(self.rows), the.aq.trainings)
  test, _    = split(test, min(500,#test))
  done,todo  = split(train, the.start)            --- [1]
  while true do
    done = keysort(done,y)
    if #done > the.Stop or #todo < 5 then break end --- [6]
    best,rest = split(done, sqrt(#done))          --- [2]
    best, rest = self:clone(best), self:clone(rest)  --- [3]
    todo = keysort(todo,br)                       --- [4]
    for _=1,2 do                                    --- [5]
      push(done, table.remove(todo));
      push(done, table.remove(todo,1)) end end
  return done, test, BR end     --- [7]

-------------------------------------------------------------------------------
big = 1E32

function adds(i,t)
  for _,x in pairs(t) do i:add(x) end; return i end

function split(t,n,     u,v)
  u,v={},{}; for j,x in pairs(t) do push(j<=n and u or v,x) end; return u,v end

function sort(t,fn) table.sort(t,fn); return t end

function kap(t,fn,     u) 
  u={}; for k,v in pairs(t) do u[1+#u] = fn(k,v) end; return u end

function map(t,fn,     u) 
  u={}; for _,v in pairs(t) do u[1+#u] = fn(v) end; return u end

function sum(t,fn,     n) 
  n=0; for _,v in pairs(t) do n=n + (fn and fn(v) or v) end; return n end

function lt(x) 
  return function(a,b) return a[x] < b[x] end end

function keysort(t,fn,     decorate,undecorate)
  decorate   = function(x) return {fn(x),x} end
  undecorate = function(x) return x[2] end
  return map(sort(map(t,decorate), lt(1)), undecorate) end

function coerce(s,     f,g) 
  f = function(s) return s=="true" and true or s ~= "false" and s end
  g = function(s) return s:match"^%s*(.-)%s*$" end
  return math.tointeger(s) or tonumber(s) or f(g(s)) end

function csv(file,fun,    s,src,t)
  if file and file ~="-" then src=io.input(file) end
  s = io.read()
  while s do
    t={}; for s1 in s:gmatch"([^,]+)" do t[1+#t]=coerce(s1) end
    fun(t) 
    s = io.read() end
  if src then io.close(src) end end

function o(x,     f,g)
  f = function(x) return #x>0 and map(x,o) or sort(kap(x,g)) end
  g = function(k,_) return string.format(":%s %s",k,o(x[k])) end 
  return type(x)=="number" and string.format("%g",x) or
         type(x)~="table"  and tostring(x) or
         (x.a or "") .. "(" .. table.concat(f(x)," ") .. ")" end

function pront(x) print(o(x)); return x end

function push(t,x) t[1+#t] = x; return x end

function new(klass, obj)
  return setmetatable(obj,klass) end

-------------------------------------------------------------------------------
local go = {}

function go.csv(_) csv(the.train, pront) end

function go.sym(_)
  pront(adds(Sym(),{"a","a","a","a","b","b","c"})) end

function go.num(_,n)
  n=Num()
  for i=1,1000 do n:add(i) end
  pront(n) end

function go.data(_,d)
  d=Data():read(the.train) 
  pront(map(d.cols.all, function(c) return {c.txt,c:mid()} end)) end

-------------------------------------------------------------------------------
local function main(      oops)
  oops = 0
  for i,s in pairs(arg) do
    s=s:sub(3)
    if go[s] then
       if go[s](arg[i+1]) == false then
          oops = oops + 1 end end end
  for x,v in pairs(_ENV) do 
    if not b4[x] then print("? ",x,type(v)) end end
  os.exit(oops) end

main()
