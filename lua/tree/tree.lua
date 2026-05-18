#!/usr/bin/env lua
local the
local help = [[
NAME
      tree.lua - num/sym stats, welford, binning, fastmap tree

  SYNOPSIS
      tree.lua [-b N] [-l N] [-s N] [--ACTION]

  OPTIONS
      -b  bins=10   bin count for Num:bin
      -l  leaf=8    stop recursion when #rows < leaf
      -p  p=2       Minkowski exponent for dist
      -s  seed=1    RNG seed

  ACTIONS
      --the         print defaults
      --all         run all egs
      --sort        sort asc/desc
      --welford     online n,mu,m2
      --mode        mode of dict
      --sd          stdev from n,m2
      --ent         shannon entropy (bits)
      --thing       str→bool/num/str coerce
      --o           pretty-print
      --csv         stream csv rows
      --Sym         symbolic col (has,mid,spread,bin)
      --Num         numeric col  (n,mu,m2,mid,spread,bin,norm)
      --gauss       1k sum-of-3 rand, mu~0 sd~1
      --head        parse header → cols {all,x,y}
      --dist        euclid over x cols
      --tree        fastmap bi-cluster
      --like        descend tree to leaf

  FLAGS
      flag = "-" + first char of key. -b 20 = bins=20.
      val coerced via thing().

  EXIT
      assert fail = nonzero. else 0.
]]
local eg,b4={},{}; for k,_ in pairs(_ENV) do b4[k] = k end
local log, seed, rand = math.log, math.randomseed, math.random
local min, max, mtype, floor, exp =
        math.min, math.max, math.type, math.floor, math.exp

-- ## lib -------------------------------------------------------
local cli,csv,ent,keysort,lt,map,mode,new,nth,o,rogues,sd,settings,sort,thing,welford
local head,adds,dist,far,poles,half,tree,like

function new(mt,t) mt.__index=mt; return setmetatable(t,mt) end

function sort(t,f) table.sort(t,f); return t end

function map(t,fn,    u) u={}; for _,v in ipairs(t) do u[1+#u]=fn(v) end; return u end

function nth(n) return function(t) return t[n] end end

function lt(n)  return function(a,b) return a[n] < b[n] end end

function keysort(t, fn,    d) d=function(x) return {fn(x),x} end
  return map(sort(map(t,d), lt(1)), nth(2)) end

function welford(v,w,n,mu,m2,     d)
  n= n+w; d= v-mu; mu= mu+w*d/n; return n,mu, m2+w*d*(v-mu) end

function mode(t,    out,N)
  N= -1; for k,n in pairs(t) do if n>N then out,N = k,n end end
  return out end

function sd(n,m2) return n<2 and 0 or (m2/(n-1))^0.5 end

function ent(t,    e,N)
  e,N= 0,0
  for _,n in pairs(t) do N = N + n end
  for _,n in pairs(t) do e = e - n/N *log(n/N,2) end 
  return e end

function thing(s)
  return s=="true" or (s ~= "false" and (tonumber(s) or s)) end

function rogues()
  for k,_ in pairs(_ENV) do 
    if not b4[k] then print("rogue?",k) end end end

function settings(s,    u)
  u={}
  for k,v in s:gmatch"[-]%S+%s+(%w+)=(%S+)" do u[k]=thing(v) end
  return u end

function o(x,     u)
  if (type(x) == "number") then
    return floor(x)==x and floor(x) or ("%.2f"):format(x) end 
  if type(x) ~= "table" then return tostring(x) end
  u = {}
  for k,v in pairs(x) do u[1+#u]=#x>0 and o(v) or k.."="..o(v)end
  return "{"..table.concat(#x>0 and u or sort(u), ", ").."}" end 

function cli(the,arg,    hit)
  for n,txt in pairs(arg) do
    if eg[txt] then eg[txt]()
    elseif txt:sub(1,1) == "-" then
      hit = false
      for key in pairs(the) do
        if txt == "-"..key:sub(1,1) then
          the[key] = thing(arg[n+1] or "")
          hit = true; break end end
      assert(hit, "bad flag: "..txt) end end end

function csv(src,  f)
  f = io.open(src)
  return function(      s,u)
    s = f:read()
    if s then
      u={}; for x in s:gmatch"[^,]+" do 
              u[1+#u] = thing(x:match"^%s*(.-)%s*$") end
      return u
    else f:close() end end end

-- ## Sym -------------------------------------------------------
local Sym={}
function Sym.new(at)      return new(Sym,{at=at, has={}}) end
function Sym.add(i,v,  w) i.has[v]=(w or 1) + (i.has[v] or 0) end
function Sym.bin(i,v)     return v end
function Sym.mid(i)       return mode(i.has) end
function Sym.spread(i)    return ent(i.has) end

-- ## Num -------------------------------------------------------
local Num={}
function Num.new(at) return new(Num,{at=at, n=0, mu=0, m2=0}) end

function Num.add(i,v,  w)
  if v=="?" then return v end
  i.n, i.mu, i.m2 = welford(v, w or 1, i.n, i.mu, i.m2) 
  return v end

function Num.mid(i)    return i.mu end
function Num.spread(i) return sd(i.n, i.m2) end

function Num.bin(i,v) 
  return v=="?" and v or floor(the.bins * i:norm(v)) end

function Num.norm(i,v)
  v= (v - i.mu) / (i:spread() + 1E-32)
  return 1 / (1 + exp(-1.7 * max(-3, min(3, v)))) end

-- ## Data ------------------------------------------------------
function head(names,    cols, col, where)
  cols = {all={}, x={}, y={}}
  for at,s in ipairs(names) do
    col      = (s:match"^[A-Z]" and Num or Sym).new(at)
    col.txt  = s
    col.goal = s:find"%+$" and 1 or s:find"%-$" and 0 or nil
    cols.all[at] = col
    if not s:find"X$" then
      where = col.goal and cols.y or cols.x
      where[1+#where] = col end end
  return cols end

function adds(cols, row)
  for _,c in ipairs(cols.all) do c:add(row[c.at]) end
  return row end

-- ## Tree ------------------------------------------------------
function dist(cols, r1, r2,    d, n, p, v1, v2)
  d, n, p = 0, 0, the.p
  for _,c in ipairs(cols.x) do
    n  = n + 1
    v1 = r1[c.at]; v2 = r2[c.at]
    if v1=="?" and v2=="?" then d = d + 1
    elseif c.mu then
      v1 = v1=="?" and (v2 > c.mu and 0 or 1) or c:norm(v1)
      v2 = v2=="?" and (v1 > 0.5  and 0 or 1) or c:norm(v2)
      d = d + math.abs(v1-v2)^p
    else
      d = d + (v1 == v2 and 0 or 1)^p end end
  return (d/n)^(1/p) end

function far(cols, rows, r1)
  return keysort(rows, function(r) return dist(cols,r1,r) end)
                [floor(#rows * 0.9)] end

function poles(cols, rows,    a, b)
  a = far(cols, rows, rows[rand(#rows)])
  b = far(cols, rows, a)
  return a, b, dist(cols, a, b) end

function half(cols, rows, a, b, c,    xs, ls, rs, m)
  xs = keysort(rows, function(r,    da, db)
    da, db = dist(cols,r,a), dist(cols,r,b)
    return (da*da + c*c - db*db) / (2*c + 1E-32) end)
  ls, rs = {}, {}; m = floor(#xs / 2)
  for i,r in ipairs(xs) do
    if i <= m then ls[1+#ls]=r else rs[1+#rs]=r end end
  return ls, rs end

function tree(cols, rows,    a, b, c, ls, rs)
  if #rows < the.leaf then return {rows=rows, leaf=true} end
  a, b, c = poles(cols, rows)
  ls, rs  = half(cols, rows, a, b, c)
  if #ls == #rows or #rs == #rows then
    return {rows=rows, leaf=true} end
  return {a=a, b=b, c=c,
          left=tree(cols, ls), right=tree(cols, rs)} end

function like(cols, node, row)
  while not node.leaf do
    node = dist(cols,row,node.a) < dist(cols,row,node.b)
             and node.left or node.right end
  return node.rows end

-- ## Eg  -------------------------------------------------------
eg["--the"] = function() print(o(the)) end

eg["--sort"] = function(    t)
  t = sort({3,1,2})
  assert(t[1]==1 and t[2]==2 and t[3]==3, "sort asc")
  t = sort({1,2,3}, function(a,b) return a>b end)
  assert(t[1]==3 and t[3]==1, "sort desc") end

eg["--welford"] = function(    n,mu,m2)
  n,mu,m2 = 0,0,0
  for _,v in ipairs{1,2,3,4,5} do
    n,mu,m2 = welford(v,1,n,mu,m2) end
  assert(n==5, "n")
  assert(mu==3, "mu")
  assert(math.abs(sd(n,m2) - 1.5811) < 1E-3, "sd") end

eg["--mode"] = function()
  assert(mode{a=1, b=5, c=2} == "b", "mode pick max") end

eg["--sd"] = function()
  assert(sd(1,99) == 0, "n<2 → 0")
  assert(math.abs(sd(5, 10) - (10/4)^.5) < 1E-9, "sd formula") end

eg["--ent"] = function(    e)
  e = ent{a=1, b=1, c=1, d=1}
  assert(math.abs(e - 2) < 1E-9, "uniform 4 → 2 bits")
  e = ent{a=1}
  assert(e == 0, "single class → 0") end

eg["--thing"] = function()
  assert(thing"true"  == true,  "true")
  assert(thing"false" == false, "false")
  assert(thing"42"    == 42,    "int")
  assert(thing"1.5"   == 1.5,   "float")
  assert(thing"hi"    == "hi",  "str")
  assert(thing""      == "",    "empty") end

eg["--o"] = function()
  assert(o(1)    == 1,        "int")
  assert(o(1.5)  == "1.50",   "float")
  assert(o"hi"   == "hi",     "str")
  assert(o{1,2,3}== "{1, 2, 3}", "list")
  assert(o{a=1,b=2} == "{a=1, b=2}", "dict sorted") end

eg["--csv"] = function(    tmp,f,rows,row)
  tmp = os.tmpname()
  f = io.open(tmp,"w"); f:write("a,b,c\n1,2,3\n"); f:close()
  rows = {}
  for r in csv(tmp) do rows[1+#rows] = r end
  os.remove(tmp)
  assert(#rows == 2, "2 rows")
  assert(rows[1][1] == "a" and rows[1][3] == "c", "header")
  assert(rows[2][1] == 1 and rows[2][3] == 3, "coerced ints") end

eg["--Sym"] = function(    s)
  s = Sym.new()
  for _,v in ipairs{"a","a","b","a","c"} do s:add(v) end
  assert(s:mid() == "a", "mode a")
  assert(s:spread() > 0, "entropy > 0")
  assert(s:bin("z") == "z", "bin identity") end

eg["--Num"] = function(    n)
  n = Num.new()
  for _,v in ipairs{1,2,3,4,5} do n:add(v) end
  assert(n:mid() == 3, "mu")
  assert(math.abs(n:spread() - 1.5811) < 1E-3, "sd")
  assert(n:add("?") == "?", "skip ?")
  assert(n.n == 5, "? not counted")
  local b = n:bin(3)
  assert(b >= 0 and b <= the.bins, "bin range") end

eg["--head"] = function(    cols)
  cols = head{"Age","name","Mpg+","Wt-","statusX"}
  assert(#cols.all == 5,         "5 cols")
  assert(#cols.x   == 2,         "Age,name = x")
  assert(#cols.y   == 2,         "Mpg+,Wt- = y")
  assert(cols.all[1].n == 0,     "Num first (Age)")
  assert(cols.all[2].has,        "Sym second (name)")
  assert(cols.all[3].goal == 1,  "Mpg+ maximize")
  assert(cols.all[4].goal == 0,  "Wt- minimize")
  assert(cols.all[5].goal == nil,"statusX ignored from xy") end

eg["--dist"] = function(    cols, d1, d2)
  cols = head{"X1","X2"}
  for _,r in ipairs{{1,1},{2,2},{3,3},{4,4},{5,5}} do adds(cols,r) end
  assert(dist(cols,{1,1},{1,1}) == 0, "self dist 0")
  d1 = dist(cols,{1,1},{2,2})
  d2 = dist(cols,{1,1},{5,5})
  assert(d2 > d1, "monotone w/ separation") end

eg["--tree"] = function(    cols, rows, root, count)
  seed(the.seed)
  cols = head{"X1","X2"}; rows = {}
  for i=1,64 do rows[i] = adds(cols, {rand(), rand()}) end
  root = tree(cols, rows)
  count = function(n) if n.leaf then return #n.rows end
                      return count(n.left) + count(n.right) end
  assert(count(root) == 64, "all rows preserved") end

eg["--like"] = function(    cols, rows, root, leaf)
  seed(the.seed)
  cols = head{"X1","X2"}; rows = {}
  for i=1,64 do rows[i] = adds(cols, {rand(), rand()}) end
  root = tree(cols, rows)
  leaf = like(cols, root, {0.5, 0.5})
  assert(#leaf < the.leaf * 2, "leaf small") end

eg["--gauss"] = function(    num)
  num = Num.new()
  for _=1,1000 do num:add(2*(rand() + rand() + rand()) - 3) end
  assert(math.abs(num:mid()) < 0.1, "mu ~ 0")
  assert(math.abs(num:spread() - 1) < 0.1, "sd ~ 1")
  print(o(num)) end

eg["--all"] = function()
  for k,fn in pairs(eg) do
    if k ~= "--all" then print("--", k); fn() end end
  print("all pass") end

-- ## Main ------------------------------------------------------
the = settings(help)
seed(the.seed)
cli(the, arg)
rogues()
