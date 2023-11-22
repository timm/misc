-- <!--  vim: set et ts=2 sts=2 sw=2 : -->
-- 1. walk table featrures in random order,
-- 2. at each step, 
--    - sample one value, find its neighborhood
--       - num neighborhood = +/i .35 *sd
--       - sym neighborhood = self
--    - set table to just the rows with that neighborhood
-- 3. when min rows found (or run out of features)
--    - interpolate between the selected rows
-- ----------------------------
-- Note: LUA's scope does not extend beyond the current line.
-- So X can not use Y unless X comes after Y (so maybe read this code bottom-up?

-- ## Config  

local the = { -----------------------------------------------------------------
     cohen=.35,     -- size of numeric neighborhood
     cf =.5, f=.3,  -- interpolation control
     want=10000,     -- how many to build
     d=2             -- default number of de 
}

-- ## Lib
-- ### Shortcuts
local big,fmt,R  --------------------------------------------------------------

big = 1E64
fmt = string.format
R   = math.random

-- ### Sorting
local shuffle, lt, sort, sorts  -----------------------------------------------

-- Fisher–Yates shuffle
function shuffle(t,   j) 
  for i=#t,2,-1 do j=R(i); t[i],t[j]=t[j],t[i] end; return t end

-- Sorting cols with some missing values
function lt(x,     g)
  q = function(x) return x=="?" and -big or x end
  return function(a,b) return q(a[x]) < q(b[x]) end end

-- soring on some function
function sort(t,fun) 
  table.sort(t,fun); return t end

-- Rows  sorted on "col"
function sorts(rows,col,nump,     u,v)  
  u,v = (nump and sort(rows,lt(col)) or rows), {}
  for _,x in pairs(u) do
    if x ~= "?" then v[1+#v] = x end end
  return v end

-- ### Random Choice  
local any  --------------------------------------------------------------------

function any(t) -- return any item
  return t[R(#t)] end

-- ### Strings to Thing
local make, csv  --------------------------------------------------------------

-- coerce string to int,floag,bool or string
function make(s,    fun) 
  function fun(s) return s=="true" or (s~="false" and s) end
  return math.tointeger(s) or tonumber(s) or fun(s:match'^%s*(.*%S)') end

-- iteratore. ead csv rows from file
function csv(src)  
  src = io.input(src)
  return function(    s,t)
    s,t = io.read(),{}
    if   s 
    then for s1 in s:gmatch("([^,]+)") do t[#t+1]=make(s1) end; return t;
    else io.close(src) end end end

-- ### Thing to  Strings 
local o, oo  ------------------------------------------------------------------

-- generate a string, round  number to `d` places, recurse into nested tables, sort hash tables on key.
function o(it,d,          u,x,mult)
  if type(it) == "number"   then
    if math.floor(it) == it then return it else 
      mult = 10^(d or the.d)
      return math.floor(it * mult + 0.5) / mult end end
  if type(it) ~= "table" then return tostring(it) end
  u={}; for k,v in pairs(it) do
          x= o(v,d)
          u[1+#u]= #it==0 and fmt(":%s %s",k,x) or x end
  return "{"..table.concat(#it==0 and sort(u) or u," ").."}" end

-- generate `it`'s string, print `it`, return `it`
function oo(it,d) print(o(it,d)); return it end

-- ## Inference


  
-- ### Nearby  
local div, nearby  ------------------------------------------------------------

-- return sd of a sorted column (assumes not missing values)
function div(rows,col) 
  return (rows[#rows*.9//1][col] - rows[#rows*.1//1])[col] / 2.56 end

-- find all rows nearby some randomly selected row. Returns two valyues marking
-- the start and top of the selection range. For non-numerics, these two values are
-- the same.
function nearby(rows,col,nump,     lo,hi,sd)
  lo = any(rows)[col]
  hi = lo
  if nump then
    sd = div(rows,col)*the.cohen
    lo,hi = lo-sd/2, lo+sd/2 end -- what to do at range overflow?
  return lo,hi end

-- ### Grow 
local grow  -------------------------------------------------------------------

-- Using Storn's DE interpolation. 
function grow(rows, numps, u, mutant)
  function mutant(nump, a, b, c, d)
    if R() > the.cf then return a end
    if not nump then return (R() > .5 and c or d) end
    return b + ((c=="?" or d=="?") and 0 or the.f(c - d))
  end -----------------------
  u = {}
  for _ = 1, the.want do
    local a, b, c, d, new = any(rows), any(rows), any(rows), any(rows), {}
    for k, v in pairs(a) do
      new[k] = mutant(numps[k], a[k], b[k], c[k], d[k])
    end
    k= R(#old)
    new[k] = old[k] -- Storn wants at least one old value in the new.
    u[1 + #u] = new
  end
  return u
end

-- ### Prune
local prune, featureOrdering, prunes  -----------------------------------------

-- Prune rows that are not nearby
function prune(rows,col,nump,     u,v,lo,hi)
  u,v   = sorts(rows,col,nump),{}
  lo,hi = nearby(rows,col,nump)
  for _,x in pairs(rows) do
    if lo <= x and x <= hi then v[1+#v] = x end end
  return v end

-- Explore features in random order
function featureOrdering(rows,    u) 
  for k,_ in pairs(rows[1]) do u[1+#u] = k end
  return shuffle(u) end

-- Prune on each feature
function prunes(rows,numps)
  for _,i in pairs(featureOrdering(rows)) do
    if #rows < the.min then break end
    rows = prune(rows,t.cols.all[i],numps[i]) end
  return grow(rows) end

--## MAIN 
local main  -------------------------------------------------------------------

function main(file,      numps,rows)
  rows={}
  for t in csv(file) do
    if numps then
      rows[1+#rows] = t  
    else
      numps = {}
      for k, v in pairs(t) do
        if v:find"^[A-Z]" then numps[k] = true end end end end 
  return prunes(rows,numps) end
