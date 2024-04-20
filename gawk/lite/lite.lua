local push
-------------------------------------------------------
local settings,the

function settings() return {
  file="../../data/auto93.csv",
  seed=1234567891,
  magic={y="[-+!]$", min="-$", num="^[A-Z]"}} end

the=settings()
-------------------------------------------------------
local col,columns,DATA,new

function DATA(strs) return {
  names=strs, 
  rows={},
  y={}, x={}, heaven={}, 
  n={}, 
  seen={}, 
  mu={}, m2={}, hi={}, lo={}}  end

function new(strs,    data) 
  data=DATA(strs)
  for c,str in pairs(strs) do 
    columns(data,c,str)
    col(data,c,str) end 
  return data end

function columns(data,c,str)
  data.n[c]=0
  if not str:find(the.magic.y) then push(data.cols.x, c) else
    push(data.cols.y, c)
    data.cols.heaven[c] = str:find(the.magic.min) and 0 or 1

function col(data,c,str)
  if not str:find(the.magic.num) then data.seen[c]={} else
    data.mu[c] = 0
    data.m2[c] = 0
    data.lo[c] =  1E30
    data.hi[c] = -1E30 end end 
-------------------------------------------------------
local add,add1,adds,nump

function adds(data,lst) for _,t in pairs(lst) do add(data,t) end; return data end

function add(data,t) 
  push(data.rows, t)
  for c,x in pairs(t) do
    if x ~= "?" then 
      data.n[c] = data.n[c]+1
      add1(data,c,x) end end end

function add1(data,c,x,    delta)
  if not nump(data,c) then data.seen[c][x] = 1 + (data.seen[c][x] or 0)  else
    delta      = x - data.mu[c]
    data.mu[c] = data.mu[c] + delta/data.n[c]
    data.m2[c] = data.m2[c] + delta*(x - data.mu[c])
    if x > data.hi[c] then data.hi[c] = x end
    if x < data.lo[c] then data.lo[c] = x end end end
  
function nump(data,c) return data.mu[c] end
#--------------------------------------------------------------
function push(t,x) t[1+#t]=x; return x end
