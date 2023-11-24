-- <!--  vim: set et ts=2 sts=2 sw=2 : -->
-- 1. Group data into regions
--    - along the way, learn most informative rows and columns.
-- 2. All inference is then  sampling each local  region.
--    - e.g. data synythesis is  
--    
-- Note: LUA's scope does not extend beyond the current line.
-- So X can not use Y unless X comes after Y (so maybe 
-- read this code bottom-up?  
local the = { -----------------------------------------------------------------
     about = {what = "giar.lua",
              why  = "example implementation, General instance-based reasoning",
              when = "(c) 2023, BSD2",
              who  = "Tim Menzies"
             },
     cohen=.35,     -- size of numeric neighborhood
     cf =.5, f=.3,  -- interpolation control
     want=10000,     -- how many to build
     d=2             -- default number of de 
}

-- ## Lib

-- ### Lint
local b4, rogues --------------------------------------------------------------

-- Cache all the names known before the code starts
b4={}; for k,v in pairs(_ENV) do b4[k]=k end

-- Report rogie locals.
-- `rogues() --> nil` 
function rogues()
 for k,v in pairs(_ENV) do 
   if not b4[k] then print("ROGUE: ",k,type(v)) end end end

-- ### Shortcuts
local big,fmt,R  --------------------------------------------------------------

big = 1E64
fmt = string.format
R   = math.random

-- ### Sorting
local shuffle, lt, sort, sorts, ordered  --------------------------------------

-- Fisher–Yates shuffle; randomizes order of a table.   
-- `shuffle([x], ?fun) --> [x]`   
function shuffle(t,   j)
  for i=#t,2,-1 do j=R(i); t[i],t[j]=t[j],t[i] end; return t end

-- Returns a function that sorts on field `x`.   
-- `lt(s) --> fun`
function lt(x,     fun)
  function fun(x) return x=="?" and -big or x end
  return function(a,b) return fun(a[x]) < fun(b[x]) end end

-- soring on some function   
-- `sort([x], ?fun) --> [x]`  
function sort(t,fun) 
  table.sort(t,fun); return t end

-- Rows  sorted on "col"   
-- `sort([x], n, b) --> [n]`
function sorts(rows,col,nump,     u,v)  
  u,v = (nump and sort(rows,lt(col)) or rows), {}
  for `,x in pairs(u) do
    if x ~= "?" then v[1+#v] = x end end
  return v end

-- Iterator. Returns key,values of `t` in key ordering.  
-- `ordered([s=x]) --> fun --> s,x`
function ordered(t,     i,u)
  i,u = 0,{}
  for k,v in pairs(t) do u[1+#u] = {k,v} end
  table.sort(u, lt(1))
  return function()
    if i < #u then
      i = i+1
      return u[i][1], u[i][2] end end end 

-- ### Random Choice  
local any  --------------------------------------------------------------------

-- Return any number.
-- `any([x]) --> x`  
function any(t) -- return any item
  return t[R(#t)] end

-- ### Strings to Thing
local coerce, csv,cli  -----------------------------------------------------------

-- Coerce string.   
-- `coerce(s) --> int | float | bool | string`
function coerce(s,    fun) 
  function fun(s) return s=="true" or (s~="false" and s) end
  return math.tointeger(s) or tonumber(s) or fun(s:match'^%s*(.*%S)') end

-- Iterator. Returns rows from a csv file.   
-- `csv(s) --> fun --> [x]
function csv(src)  
  src = io.input(src)
  return function(    s,t)
    s,t = io.read(),{}
    if   s 
    then for s1 in s:gmatch("([^,]+)") do t[#t+1]=coerce(s1) end; return t;
    else io.close(src) end end end

-- Update settings from command line; e.g. for key `k`, look for `--k v`
-- on the command line. Boolean flags do not head arguments
-- (we just invert the default).   
-- `cli([k=v]) --> [k=v]`
function cli(t)
  for k,v in pairs(t) do
    k = tostring(k)
    for pos,flag in pairs(arg) do
      if flag=="--"..k then
        v = (v=="true" and "false") or (v="false" and "true") or arg[pos+1]
        t[k] = thing(v)  end end end
  return t end 

-- ### Thing to  Strings 
local o, oo  ------------------------------------------------------------------

-- Return an integer for simple numbers, else round `n` to `d` places (default=2).   
-- `rnd(n,?d) --> n`
function rnd(n)
  if math.floor(it) == it then return it else 
  mult = 10^(d or the.d or 2)
  return math.floor(it * mult + 0.5) / mult end end

-- Generate a string, from `x`. round  number to `d` places, recurse into nested tables, sort hash tables on key.    
-- `o(x,n) --> s`     
function o(it,d,          u,fun)
  function fun(k,x) return #it==0 and fmt(":%s %s",k,x) or x end
  if type(it) == "number" then return rnd(it,d) end
  if type(it) ~= "table"  then return tostring(it) end
  u={}; for k,v in ordered(it) do u[1+#u] = fun(k, o(v,d)) end
  return "{"..table.concat(u," ").."}" end

-- generate `it`'s string, print `it`, return `it`   
-- `oo(x,n) --> [x]`
function oo(it,d) print(o(it,d)); return it end

-- ## Inference

-- ### Nearby  
local div, nearby  ------------------------------------------------------------

-- return sd of a sorted column (assumes not missing values).   
-- `div([[x]], n) --> n`
function div(rows,col) 
  return (rows[#rows*.9//1][col] - rows[#rows*.1//1])[col] / 2.56 end

-- find all rows nearby some randomly selected row. Returns two valyues marking
-- the start and top of the selection range. For non-numerics, these two values are
-- the same.    
-- `nearby([[x]],n,bool)` --> x,x`
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
-- `grow( [[x]], [k=v]) --> [[x]]`
function grow(rows, numps,      u, mutant)
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

-- Prune rows that are not nearby.    
-- ``print([[x]],n,bool)`
function prune(rows,col,nump,     u,v,lo,hi)
  u,v   = sorts(rows,col,nump),{}
  lo,hi = nearby(rows,col,nump)
  for _,x in pairs(rows) do
    if lo <= x and x <= hi then v[1+#v] = x end end
  return v end

-- Explore features in random order.   
-- ``featureOrdering( [[x]]) --> [[x]]`
function featureOrdering(rows,    u) 
  for k,_ in pairs(rows[1]) do u[1+#u] = k end
  return shuffle(u) end

-- Prune on each feature    
-- ``print( [[x]]],[k=x]) --> [[x]]`
function prunes(rows,numps)
  for _,i in pairs(featureOrdering(rows)) do
    if #rows < the.min then break end
    rows = prune(rows,t.cols.all[i],numps[i]) end
  return rows end

--## MAIN 
local main  -------------------------------------------------------------------

-- Read data from disc, update `the` from clu,   
-- `main(s) --> [[x]]`
function main(file,      numps,rows)
  rows={}
  for t in csv(file) do
    if numps then
      rows[1+#rows] = t  
    else
      numps = {}
      for k, v in pairs(t) do
        if v:find"^[A-Z]" then numps[k] = true end end end end 
  out= grow(prunes(rows,numps),numps)
  rogues()
  return out end
