local the,big,abs,min,max,log,s2n,s2a,atom,push
local adds,sort,shfflemap,kap,atom,kap,o,csv,new,sampler
local Sym,Num,Data,Cols,Freq = {},{},{},{},{}

the = {Acq    = "xploit",
       assume = 4,
       Bins   = 7,
       build  = 20,
       check  = 5,
       file   = "../../moot/optimize/misc/auto93.csv",
       k      = 2,
       m      = 1,
       p      = 2, 
       seed   = 1234567891}

function new(kl,t) kl.__index=kl; return setmetatable(t,kl) end

-------------------------------------------------------------------------------
function Sym:new(at,txt)
  return new(Sym, {at=at or 0, txt=txt or "", n=0, has={}}) end

function Sym:add(x)
  if x ~= "?" then
    self.n = self.n + 1
    self.has[x] = (self.has[x] or 0) + 1 end end

function Sym:bin(x) return x end

-------------------------------------------------------------------------------
function Num:new(at,txt) -- (int,str) --> Num
  return new(Num, {at=at or 0, txt=txt or "", n=0, lo=big, hi=-big,
                   heaven = tostring(txt or ""):find"-$" and 0 or 1 }) end

function Num:add(x)
  if x ~= "?" then
    self.n = self.n + 1
    self.lo = min(x, self.lo)
    self.hi = max(x, self.hi) end end

function Num:bin(x) 
  return min(the.bins - 1, self:norm(x) * the.Bins) // 1) end

function Num:norm(x)
  return (x-self.lo)/ (self.hi - self.lo + 1/big) end

-------------------------------------------------------------------------------
function Cols:new(t,      all,x,y,klass,col)
  all, x, y = {},{},{}
  for n,s in pairs(t) do
    col = push(all, (s:find"^[A-Z]" and Num or Sym):new(n,s))
    if not s:find"X$" then
      push(s:find"[+!-]$" and y or x,col)
      if s:find"!$" then klass = col end end end 
  return new(Cols, {x=x,y=y,all=all,names=t, klass=klass}) end

function Cols:add(t)
  for _,col in pairs(self.all) do col:add(t[col.at]) end
  return t end

-------------------------------------------------------------------------------
function Data:new(t)
  return adds(t, new(Data, {rows={}, cols=nil})) end

function Data:add(t)
  if   self.cols then push(self.rows,self.cols:add(t)) 
  else self.cols = Cols(t) 
  end
  return self end

function Data:ydist(row)
   d,n = 0,0
   for _,col in pairs(self.cols.y) do
     n = n + 1
     d = d + abs(col:norm(row[col.at]) - col.heaven)^2 end
   return (d/n) ^ 0.5 end

function Data:clone(t)
  return adds(t, Data:new({self.cols.names})) end

-------------------------------------------------------------------------------
function Freq:new(d)
   return new(Freq,{nall=0, nh=0, fs={}, nk={}, data=d}) end

function Freq:add(row,k, inc,         v)
  inc        = inc or 1
  self.nall  = self.nall + inc
  self.nh    = self.nh + (self.nk[k] and 0 or 1)
  self.nk[k] = (self.nk[k] or 0) + inc
  for _,col in pairs(self.data.x) do
    v=row[col.at]
    if v ~= "?" then
      assign3(self.fs, k, col.at, col:bin(v),inc) end end end 

function Freq:like(row,k,      prior,l,v,f)
  prior = (self.nk[k] + the.k) / (self.nall + the.k*self.nh)
  l = log(prior)
  for _,col in in pairs(self.data.x) do 
    v = row[col.at]
    if v ~= "?" then
      f = access3(self.fs, k, col.at, col:bin(v))
      l = l + log((f + the.m*prior)/(nk + the.m + 1/big)) end end 
  return l  end

function Freq:acquire(row,            b,r,p,q)
   b, r = self:like(row, true), self:like(row, false)
   b, r = math.e**b, math.e**r
   p    = self.nall / the.Build
   q    = {"xploit"=0, "xplor"=1}[the.Acq] or (1 - p)
   return (b + r*q) / abs(b*q - r + 1/big) end

function Data:acquires(rowas):
   Y= function(r) return self:ydist(r) end
   YR=function(r) return {-Y(r), r} end
   any = sampler(rows)
   seen = self:clone(any(4))
   seen.rows,Y)

---------------------------------------------------------------------------
big  = 1E32
abs  = math.abs
log  = math.log
min  = math.min
max  = math.max

trim = function(s) return s:match"^%s*(.-)%s*$" end
s2n  = function(s) return tonumber(s) or math.tointeger(s) end
s2a  = function(s) return (s=="true" and true) or (s~= "false" and s) end
atom = function(s) return s2n(s) or s2a(trim(s)) end

function atoms(s,      t)
  t={}; for x in s:gmatch("([^,]+)") do t[1+#t]=atom(x) end; return t  end

function csv(file,     src,_atoms) 
  src = io.input(file)
  return function(    s) 
    s = io.read()
    if s then return atoms(s) else io.close(src) end end end 

function cli(t)
  for k,v in pairs(t) do
    v = tostring(v)
    for argv,s in pairs(arg) do
      if s=="-"..(k:sub(1,1)) or s==("-"..k) then
        v = v=="true" and "false" or v=="false" and "true" or arg[argv+1]
        t[k] = atom(v) end end  end 
  return t end 

function kap(t,fn,    u) 
  u={}; for k,v in pairs(t) do u[1+#u] = fn(k,v) end; return u end  

map  = function(t,fn) return kap(t,function(_,v) return fn(v) end) end
push = function(t,x)  t[1+#t]=x; return x end

lt   = function(x) return function(t,u) return t[x] < u[x] end end
gt   = function(x) return function(t,u) return t[x] > u[x] end end
sort = function(t,fn) table.sort(t,fn); return t end

function o(x,     _kv,_fmt,_yes)
  _fmt= string.format
  _yes= function(k) return tostring(k):sub(1,1)~="_" end 
  _kv = function(k,v) if _yes(k) then return _fmt(":%s %s",o(k),o(v)) end end
  return type(x)=="number" and _fmt(x//1==x and "%s" or "%.3g", x) or 
         type(x)~="table"  and tostring(x) or
         "{"..table.concat(#x>0 and map(x,o) or sort(kap(x,_kv)),", ").."}" end

function access3(t,x,y,z,    a,b)
  a = t[x]; if a==nil then return 0 end
  b = a[y]; if b==nil then return 0 end
  return b[z] or 0 end

function assign3(t,x,y,z,  inc,       a,b)
  a = t[x];  if a==nil then a={}; t[x] = a end
  b = a[y];  if b==nil then b={}; a[y] = b end
  b[z] = (b[z] or 0) + (inc or 1) end

function adds(t,i)
  for _,x in pairs(t or {}) do
     i = i or (type(x)=="number" and Num or Sym)()
     i:add(x) end
  return i end


function shuffle(t,   j) 
   for i=#t,2,-1 do j=math.random(i);t[i],t[j]=t[j],t[i] end
   return t end

function sampler(t,    i,n)
  i, n = 1, #t
  shuffle(t)
  return function(m,     out)
    local out = {}
    for _ = 1, m do
      if i > n then shuffle(t); i = 1 end
      out[#out+1] = t[i]; i = i + 1 end
    return out end end

-------------------------------------------------------------------------------
local eg={}

eg["--the"] = function() print(o(the)) end

eg["--csv"] = function() 
  for t in csv(the.file) do print(o(t)) end end

-------------------------------------------------------------------------------
if arg[0]:find"am.lua$" then
  the = cli(the)
  for n,s in pairs(arg) do
    if eg[s] then 
      math.randomseed(the.seed)
      eg[s]() end end end
