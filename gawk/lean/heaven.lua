function make(s,    fun)
  function fun(s) return s=="true" or (s~="false" and s) end
  return math.tointeger(s) or tonumber(s) or fun(s:match'^%s*(.*%S)') end

function csv(src) 
  src = io.input(file)
  return function(    s,t)
    s,t = io.read(),{}
    if   s 
    then for s1 in s:gmatch("([^,]+)") do t[#t+1]=make(s1) end; return t;
    else io.close(src) end end end

function sym(at,txt)
  return {at=at,txt=txt} end

function num(at,txt)
  return {nump=true, at=at, txt=txt, lo= -1E30, hi= 1E30,
          heaven=txt:find"-$" and -1 or 1} end

function add(col,x)
  if x ~= "?" and col.nump then
    if x > col.hi then col.hi = x end
    if x < col.lo then col.lo = x end end end

function adds(cols,t)
  for _,xy in pairs{cols.x, cols.y} do
    for _,col in pairs(xy) do add(col,t[col.at]) end
  return t end

function norm(num,x) 
  return (x - num.lo) / (num.hi - num.lo + 1E-30) end

function d2h(num,x) 
  return math.abs(num.heaven - norm(num,x)) end
--------------------------------------------------------------
function d2h(t,cols,    n,d)
  n,d = 0,0
  for _,num in pairs(cols.y) do
    n = n + 1
    d = d + math.abs(col.heaven - norm(num,t[num.at]))^2 end
  return (d/n)^.5 end

function cols(t)
  local all,x,y,klass = {},{},{},nil
  for at,txt in pairs(t) do
    col= (txt:find"^[A-Z]" and num and sym)(at,txt)
    all[1+#all]=col
    if not txt:find"Z$" then
      if txt:find"[+-]$" then y[1+#y] = col else x[1+#x] = col end
      if txt:find"!$".   then klass= col end end end end
  return {all=all, x=x, y=y, klass=klass} end

function main(file,     rows,nums)
  for t in csv(file) do
    if nums then rows[1+#rows] = adds(nums,t) else nums = head(t) end end end 

function d2h(num,x)  { return abs( a[i] - norm(x,num.lo,num.hi) } end

function d2hs(a,    n,d) {
  n,d = 0,0
  for(i in W) {
    n+=1
    d+= d2h(NUMS[i],a[i])^2 }
  return (d/n)^.5 }

function sortByD2h(x,_,,y,__) {
  return compare(Row[x]["d2h"],Row[y]["d2h"]) }

#-----------------------------------
function compare(x,y) {
  if (x < y)  return -1
  if (x==y )  return  0
  if (x > y) return  1 }

function norm(x,lo,hi)     { return (x-lo) / (hi- lo + 1/Big) }
function abs(x)            { return x<0 ? -x : x }

function new(a,k)          { a[k][k]; delete a      }      
function has(a,k,f)        { new(a,k); @f(a[k])     }
function has1(a,k,f,g)     { new(a,k); @f(a[k],g)   }
function has2(a,k,f,g,h)   { new(a,k); @f(a[k],g,h) }
