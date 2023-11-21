--[[ 
1. walk table featrures in random order,
2. at each step, 
   - sample one value, find its neighborhood
      - num neighborhood = +/i .35 *sd
      - sym neighborhood = self
   - set table to just the rows with that neighborhood
3. when min rows found (or run out of features)
   - interpolate between the selected rows
--]]

-- CONFIG -----------------------------------    
the={cohen=.35,     -- nuneric neighborhood
     cf =.5, f=.3,  -- interpolation control
     want=10000,     -- how many to build
     d=2             -- default number of de 
}

-- SHORTCUTS -----------------------------------
big = 1E64
fmt = string.format
R   = math.random

-- SORTING -----------------------------------
function shuffle(t,   j) -- Fisher–Yates shuffle
  for i=#t,2,-1 do j=R(i); t[i],t[j]=t[j],t[i] end; return t end

function lt(x,     g)  -- sorting cols with some missing values
  function q(x) return x=="?" and -big or x end
  return function(a,b) return q(a[x]) < q(b[x]) end end

function sort(t,fun) -- soring on some function
  table.sort(t,fun); return t end

function sorts(t,col,nump,     u,v)  --> rows, soroted on "col"
  u,v = (nump and sort(t,lt(col)) or t), {}
  for _,x in pairs(u) do
    if x ~= "?" then v[1+#v] = x end end
  return v end

-- SELECT -----------------------------------
function any(t) -- return any item
  return t[R(#t)] end

-- STRINGS -> THING -----------------------------------
function make(s,    fun) -- coerce string to int,floag,bool or string
  function fun(s) return s=="true" or (s~="false" and s) end
  return math.tointeger(s) or tonumber(s) or fun(s:match'^%s*(.*%S)') end

function csv(src)  -- iteratore. ead csv rows from file
  src = io.input(src)
  return function(    s,t)
    s,t = io.read(),{}
    if   s 
    then for s1 in s:gmatch("([^,]+)") do t[#t+1]=make(s1) end; return t;
    else io.close(src) end end end

-- THING -> STRINGS -----------------------------------
fmt=string.format
function o(t,d,          u,x,mult)
  if type(t) == "number"   then
    if math.floor(t) == t then return t else 
      mult = 10^(d or the.d)
      return math.floor(t * mult + 0.5) / mult end end
  if type(t) ~= "table" then return tostring(t) end
  u={}; for k,v in pairs(t) do
          x= o(v,d)
          u[1+#u]= #t==0 and fmt(":%s %s",k,x) or x end
  return "{"..table.concat(#t==0 and sort(u) or u," ").."}" end

function oo(t,d) print(o(t,d)); return t end

-- INTERPOLATION --------------------------------------
function grow(t,numps,    u,mutant)
  function mutant(nump, a,b,c,d)
    if R() > the.cf then return a end
    if not nump     then return (R()> .5 and c or d) end
    return b + the.f(c-d) end
  -------------  
  u={}
  for _ =1,the.want do
    local a,b,c,d,new = any(t), any(t), any(t), any(t),{}
    for k,v in pairs(a) do
      new[k] = mutant(numps[k],a[k],b[k],c[k],d[k]) end
    u[1+#u]= new end
  return u end
  
-- NEARBY --------------------------------------
function div(t,col) -- sd of a sorted column
  return (t[#t*.9//1] - t[#t*.1//1]) / 2.56 end

function nearby(t,col,nump,     x,y,sd)
  x = any(t)[col] 
  if nump then
    sd = div(t,col)*the.cohen
    x,y = x-sd/2, x+sd/2 end -- what to do at range overflow?
  return x,(y or x) end

-- DOWN SELECT ON FEATURES ----------------------
function prune(t,col,nump,     u,v)
  u,v   = sorts(t,col,nump),{}
  lo,hi = nearby(t,col,nump)
  for _,x in pairs(t) do
    if lo <= x and x <= hi then v[1+#v] = x end end
  return v end

function featureOrdering(t,    u) -- random feature ordering
  for k,_ in pairs(t[1]) do u[1+#u] = k end
  return shuffle(u) end

function prunes(t,numps)
  for _,i in pairs(featureOrdering(t)) do
    if #t < the.min then break end
    t= prune(t.rows,t.cols.all[i],numps[i]) end
  return grow(t) end

-- MAIN ---------------------
function main(file,      numps,rows)
  numps,rows={},{}
  for t in csv(file) do
    if not numps then
     for k, v in pairs(t) do
        if v:find"^[A-Z]" then numps[k] = true end end
    else rows[1+#rows] = t  end
  return prunes(t,numps) end