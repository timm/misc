big=1E30

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

function num(at,txt)
  return {at=at, txt=txt, w=txt:find"-$" and -1 or 1, lo= -1E30, hi= 1E30} end

function add(num,x)
  if x ~= "?" then
    if x > num.hi then num.hi = x end
    if x < num.lo then num.lo = x end end end

function norm(num,x)
  return (x - num.lo) / (num.hi - num.lo + 1E-30) end
  
function body(nums,t)
  for at,num in pairs(nums) do add(num,t[at]) end
  return t end

function main(file,     rows,nums)
  for t in csv(file) do
    if nums then rows[1+#rows] = body(nums,t) else nums = head(t) end end end 

function d2h(num,x)  { return abs( a[[i] - norm(x,num["lo"],num["hi"]))  }

function d2hs(a,    n,d) {
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
