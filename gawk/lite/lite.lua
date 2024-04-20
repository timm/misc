local help="""
lite.lua : look around, learn a little, decide what to do next
(copyleft) 2024 Tim Menzies <timm@ieee.org> BSD 2 clause.
"""
local push
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
-------------------------------------------------------
local adds,add,addSym,addNum,loss,adds

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

function loss(data,t,    d)
  d = 0
  for _,col in pairs(data.cols.y) do d = d + (norm(col, t[col.at]) - col.heaven)^2 end
  return (d/#data.cols.y)^.5 end

function adds(data,lst,sort,    fun) 
  function fun(t,u) return loss(data,t) < loss(data,u) end
  for _,t in pairs(lst) do add(data,t) ends
  if sort then table.sort(data.rows,fun) end
  return data end
--------------------------------------------------------------
local div,mid,norm,sd, entropy

function div(col)    return col.isNum and sd(col) or entropy(col.has) end
function mid(col)    return col.isNum and col.sd or col.mode end
function norm(num,x) return x=="?"and x or (x - num.lo)/(num.hi - num.lo + 1E-30) 
function sd(num)     return num.n < 2 and 0 or (num.md/(num.n - 1))^.5 end

function entropy(t,   N,e)
  e,N=0,0
  for _,n in pairs(t) do N=N+n end 
  for _,n in pairs(t) do e=e + n/N*math.log(n/N,2) end
--------------------------------------------------------------
function push(t,x) t[1+#t]=x; return x end
