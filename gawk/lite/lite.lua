local push
-------------------------------------------------------
local settings,the

function settings() return {
  file="../../data/auto93.csv",
  seed=1234567891,
  magic={y="[-+!]$", min="-$", num="^[A-Z]"}} end

the=settings()
-------------------------------------------------------
local DATA, newSym, newNum

function DATA(strs,    data) 
  data = {names=strs, rows={}, x={}, y={}, syms={}, nums={}} 
  for c,str in pairs(strs) do 
    push(str:find(the.magic.y) and data.y or data.x, c)
    (str:find(the.magic.num) and newNum or newSym)(data,c,str) end 
  return data end

function newSym(data,c,_) 
  data.syms[c]={} end

function newNum(data,c,str) 
  data.nums[c] = {heaven=str:find(the.magic.min) and 0 or 1,
                  n=0,mu=0,m2=0,lo=1E30, hi=-1E30} end
-------------------------------------------------------
local add,addSym,addNum,adds

function adds(data,lst) 
  for _,t in pairs(lst) do add(data,t) end; return data end

function add(data,t) 
  push(data.rows, t)
  for c,x in pairs(t) do
    if x ~= "?" then 
      if data.nums[c] then addNum(data.nums[c],x) else addSym(data.syms[c],x) end end end end

function addSym(sym,x) 
  sym[x] = 1 + (sym[x] or 0)  end

function addNum(num,x,     delta)
  num.n  = num.n + 1
  delta  = x - num.mu
  num.mu = num.mu + delta/num.n
  num.m2 = num.m2 + delta*(x - num.mu)
  if x > num.hi then num.hi = x end
  if x < num.lo then num.lo = x end end 
#--------------------------------------------------------------
function push(t,x) t[1+#t]=x; return x end
