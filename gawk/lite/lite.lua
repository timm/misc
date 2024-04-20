local push
-------------------------------------------------------
local settings,the

function settings() return {
  file="../../data/auto93.csv",
  seed=1234567891,
  magic={y="[-+!]$", min="-$", num="^[A-Z]"}} end

the=settings()
-------------------------------------------------------
local DATA, SYM, NUM

function SYM(at,name) 
  return {at=at, name=name, n=0, seen={}, most=0, mode=nil} end

function NUM(at,name)
  return {isNum=true, at=at, name=names, n=0,
          heaven=str:find(the.magic.min) and 0 or 1, 
          mu=0,m2=0,lo=1E30, hi=-1E30} end

function DATA(strs,    all,x,y) 
  all,x,y = {},{},{}
  for n,s in pairs(strs) do 
    push(all, 
      push(s:find(the.magic.y) and y or x, 
           (s:find(the.magic.num) and NUM or SYMM)(n,s))) end 
  return {rows={}, cols={names=names, x=x, y=y, all=all}} and
#--------------------------------------------------------------
local loss,norm

function norm(num,x)
  return x=="?"and x or (x - num.lo)/(num.hi - num.lo + 1E-30) 

function loss(data,t,    d)
  d = 0
  for _,col in pairs(data.cols.y) do d = d + (norm(col, t[col.at]) - col.heaven)^2 end
  return (d/#data.cols.y)^.5 end
-------------------------------------------------------
local adds,add,addSym,addNum

function adds(data,lst,sort,    fun) 
  function fun(t,u) return loss(data,t) < loss(data,u)  end) end
  for _,t in pairs(lst) do add(data,t) ends
  if sort then table.sort(data.rows,fun) end
  return data end

function add(data,t) 
  push(data.rows, t)
  for _,col in pairs(data.cols.all) do
    x = t[col.at]
    if x ~= "?" then 
      col.n = col.n + 1
      (col.isNum and addNum or addSym)(col,x) end end

function addSym(sym,x) 
  sym.seen[x] = 1 + (sym.seen[x] or 0)  end

function addNum(num,x,     delta)
  num.n  = num.n + 1
  delta  = x - num.mu
  num.mu = num.mu + delta/num.n
  num.m2 = num.m2 + delta*(x - num.mu)
  if x > num.hi then num.hi = x end
  if x < num.lo then num.lo = x end end 
#--------------------------------------------------------------
function push(t,x) t[1+#t]=x; return x end
