local the = {
  train="../../data/auto93.csv",
  seed=1234567891,
  max=256}

math.randomseed(the.seed)

local Cols,Data,Num,Sym = {},{},{},{}
local adds,push,coerce,cells,o,new
local ok,norm,mid,per,hi,lo,div,mids,csv

--------------------------------------------------------------------------

function Num:new(s,at)   
  return new(Num,  {n=0, txt=s, at=at, has={}, max=128, ok=false}) end

function Sym:new(s,at)   
  return new(Sym,  {n=0, txt=s, at=at, has={}, mode=nil, most=0}) end

function Data:new(names) 
  return new(Data, {cols=Cols(names), rows={}}) end

function Cols:new(names,      it,col)
  it = new(Cols, {names=names, all={}, x={}, y={}, klass=nil})
  for at,s in pairs(it.names) do
    col = push(it.all, (s:find"^[A-Z]" and Num or Sym):new(s,at))
    if not s:find"X$" then
      if s:find"!$" then it.klass = col end
      push(s:find"[!+-]$" and it.y or it.x, col) end end
  return it end

function Data:clone(rows) return adds(rows or {}, Data(self.cols.names)) end

--------------------------------------------------------------------------
function Num:add(x,   pos)
  if x ~= "?" then
    self.n = self.n + 1
    if #self.has < self.max then pos=#self.has+1 
    elseif math.random() < #self.has/self.n then pos=math.random(#self.has) end
    if pos then
      self.has[pos] = x
      self.ok = false end end end

function Sym:add(x)
  if x ~= "?" then
    self.n = self.n + 1
    self.has[x] = (self.has[x] or 0) + 1 
    if self.has[x] > self.most then self.most,self.mode = self.has[x],x end end end

function Data:add(row)
  push(self.rows, row)
  for _,col in pairs(self.cols.all) do col:add(row[col.at]) end
  return self end

function adds(t,it)
  if type(t[1])=="number" then it=Num() end
  if type(t[1])=="string" then it=Sym() end
  for _,x in pairs(t) do it:add(x) end; return it end

local function reads(        data1) 
  for _,row1 in csv() do 
    if data1 then data1:add(row1) else data1=Data:new(row1) end end 
  return data1 end

--------------------------------------------------------------------------
function ok(it) if not it.ok then table.sort(it.has); it.ok=true end; return it end

function norm(it,x)      return x=="?" and x or (x - lo(it))/(hi(it) - lo(it) + 1E-32) end
function mid(it)         return it.mode and it.mode or per(it,0.5) end
function per(it,n)       return ok(it).has[#it.has*n//1] end
function hi(it)          return ok(it).has[#it.has] end
function lo(it)          return ok(it).has[1] end
function div(it,      e) 
  if it.mode 
  then e=0; for _,n in pairs(it.has) do e = e - n/it.n*math.log(n/it.n,2) end; return e 
  else return (per(it,0.9) - per(it,0.1)) / 2.58 end end

function mids(data1,    t)
  t={}; for _,col in pairs(data1.cols.all) do push(t, mid(col)) end; return t end

------------------------------------------------------------------------------------------
function push(t,x) t[1+#t]=x; return x end

function coerce(s)
  return s=="?" and s or math.tointeger(s) or tonumber(s) or 
         s=="true" and true or s ~= "false" and s end

function cells(s,     t)
  t={}
  for s1 in s:gmatch"([^,]+)" do t[1+#t] = coerce(s:match"^%s*(.-)%s*$") end
  return t end 

function csv()
  return function(    s)
    s = io.read()
    if s then return cells(s) end end end

function o(t,     u,fmt)
  fmt=string.format
  if type(t) == "number" then 
    return t//1 == t and tostring(t//1) or fmt("%.3f",t) end
  if type(t) ~= "table"  then return tostring(t) end
  u={}; for k,v in pairs(t) do u[1+#u] = #t==0 and fmt(":%s %s",k, o(v)) or o(v) end
  if #t==0 then table.sort(u) end
  return "{" .. table.concat(u," ") .. "}" end

function new(kl,t)
  kl.__index=kl; kl.__tostring = o; return setmetatable(t,kl) end
