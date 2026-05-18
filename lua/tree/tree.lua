#!/usr/bin/env lua
-- luacheck: ignore 211 212 213 311 312 421 422 431 432 611 612 613 614 631
local the,help = {},[[
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
      --relevant    descend tree to leaf
      --cut         Sym/Num candidate split values
      --divides     split rows by (col,v) → left,right,summaries
      --cuts        best (col,v) by weighted spread of y
      --show        print tree as if/else rules

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
local cells,cli,csv,cuts,divides,dist,ent,far,half,head,keysort,lt
local kap,map,mode,new,nth,o,poles,relevant,rogues,sd
local push,settings,show,slices,sort,thing,tree,values,welford

function new(mt,t) mt.__index=mt; return setmetatable(t,mt) end

function sort(t,f) table.sort(t,f); return t end

function push(t,x) t[1+#t]=x; return x end

function map(t,fn,    u)
  u={}; for _,v in ipairs(t) do u[1+#u]=fn(v) end; return u end

function kap(t,fn,    u)
  u={}; for k,v in pairs(t) do u[1+#u]=fn(k,v) end; return u end

function values(col, rows,    u)
  u={}; for _,r in pairs(rows) do
          if r[col.at] ~= "?" then u[1+#u] = r[col.at] end end
  return u end

function slices(t, n,    u, step)
  sort(t); u, step = {}, #t / n
  for k=1, n-1 do u[k] = t[floor(k*step)] end
  return u end

function nth(n) return function(t) return t[n] end end

function lt(n)  return function(a,b) return a[n] < b[n] end end

function keysort(t, fn,    d) 
  d=function(x) return {fn(x),x} end
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

function o(x,    u,kv)
  if type(x) == "number" then
    return floor(x)==x and floor(x) or ("%.2f"):format(x) end
  if type(x) ~= "table" then return tostring(x) end
  kv = function(k,v) return k.."="..o(v) end
  u = #x>0 and map(x, o) or sort(kap(x, kv))
  return "{"..table.concat(u, ", ").."}" end

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
function Sym.new(s,at)    
  return new(Sym,{at=at or 0, txt=s or "", has={}}) end

function Sym.add(i,v,  w) i.has[v]=(w or 1) + (i.has[v] or 0) end
function Sym.bin(i,v)     return v end
function Sym.mid(i)       return mode(i.has) end
function Sym.spread(i)    return ent(i.has) end

function Sym.cut(i, rows,    u)
  u = {}; for _,v in pairs(values(i, rows)) do u[v] = v end
  return kap(u, function(k) return k end) end

-- ## Num -------------------------------------------------------
local Num={}
function Num.new(s,at)
  return new(Num,{at=at or 0, txt=s or "", n=0, mu=0, m2=0,
                  goal=(s or ""):find"-$" and 0 or 1}) end

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

function Num.cut(i, rows)
  return slices(values(i, rows), the.bins) end

function divides(col, v, rows, y,    ls,rs,lsum,rsum,x,match)
  match = col.mu and function(x) return x <= v end
                  or function(x) return x == v end
  ls, rs, lsum, rsum = {}, {}, Sym.new(), Sym.new()
  for _,r in ipairs(rows) do x=r[col.at]
    if x~="?" then
      if match(x) then ls[1+#ls]=r; lsum:add(y(r))
      else             rs[1+#rs]=r; rsum:add(y(r)) end end end
  return ls, rs, lsum, rsum,
         col.mu and "<=" or "==", col.mu and ">" or "~=", v end

-- ## Data ------------------------------------------------------
function head(names,    col, all, x, y, klass)
  all, x, y = {}, {}, {}
  for at,s in ipairs(names) do
    col = push(all, (s:match"^[A-Z]" and Num or Sym).new(s,at))
    if not s:find"X$" then 
      push(s:find"[-+!]$" and y or x, col) 
      if s:find"!$" then klass = col end end end
  return {all=all, x=x, y=y, klass=klass} end

function cells(cols, row)
  for _,c in ipairs(cols.all) do c:add(row[c.at]) end
  return row end

function cuts(cols,rows,y,    
              best,score,ls,rs,lsum,rsum,op1,op2,s,nl,nr)
  best, score = nil, math.huge
  for _,c in pairs(cols.x) do
    for _,v in pairs(c:cut(rows)) do
      ls, rs, lsum, rsum, op1, op2 = divides(c, v, rows, y)
      nl, nr = #ls, #rs
      if nl > 0 and nr > 0 then
        s = (nl * lsum:spread() + nr * rsum:spread()) / (nl + nr)
        if s < score then
          score = s
          best= {col=c, v=v, op1=op1, op2=op2} end end end end
  return best end

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

function far(cols, rows, r1,     d)
  d = function(r) return dist(cols,r1,r) end
  return keysort(rows, d)[floor(#rows * 0.9)] end

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

function tree(cols, rows,    a,b,c, ls,rs, east, y, best)
  if #rows >= the.leaf then
    a, b, c = poles(cols, rows)
    ls, rs  = half(cols, rows, a, b, c)
    if #ls < #rows and #rs < #rows then
      east = {}; for _,r in ipairs(ls) do east[r] = true end
      y    = function(r) return east[r] and "e" or "w" end
      best = cuts(cols, rows, y)
      if best then
        best.left  = tree(cols, ls)
        best.right = tree(cols, rs)
        return best end end end
  return {rows=rows, leaf=true} end

function relevant(cols, node, row,    x, go)
  while not node.leaf do
    x  = row[node.col.at]
    if x == "?" then return node.rows end
    go = node.col.mu and x <= node.v or x == node.v
    node = go and node.left or node.right end
  return node.rows end

function show(node, pre)
  pre = pre or ""
  if node.leaf then print(pre.."#"..#node.rows); return end
  print(pre..node.col.txt.." "..node.op1.." "..o(node.v))
  show(node.left, pre.."| ")
  print(pre..node.col.txt.." "..node.op2.." "..o(node.v))
  show(node.right, pre.."| ") end

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
  for _,r in ipairs{{1,1},{2,2},{3,3},{4,4},{5,5}} do cells(cols,r) end
  assert(dist(cols,{1,1},{1,1}) == 0, "self dist 0")
  d1 = dist(cols,{1,1},{2,2})
  d2 = dist(cols,{1,1},{5,5})
  assert(d2 > d1, "monotone w/ separation") end

eg["--tree"] = function(    cols, rows, root, count)
  seed(the.seed)
  cols = head{"X1","X2"}; rows = {}
  for i=1,64 do rows[i] = cells(cols, {rand(), rand()}) end
  root = tree(cols, rows)
  count = function(n) if n.leaf then return #n.rows end
                      return count(n.left) + count(n.right) end
  assert(count(root) == 64, "all rows preserved") end

eg["--relevant"] = function(    cols, rows, root, leaf)
  seed(the.seed)
  cols = head{"X1","X2"}; rows = {}
  for i=1,64 do rows[i] = cells(cols, {rand(), rand()}) end
  root = tree(cols, rows)
  leaf = relevant(cols, root, {0.5, 0.5})
  assert(#leaf < the.leaf * 2, "leaf small") end

eg["--show"] = function(    cols, rows, root)
  seed(the.seed)
  cols = head{"X1","X2"}; rows = {}
  for i=1,32 do rows[i] = cells(cols, {rand(), rand()}) end
  root = tree(cols, rows)
  show(root) end

eg["--cut"] = function(    cols, rows, us)
  cols = head{"X1","name"}; rows = {}
  for _,r in ipairs{{1,"a"},{2,"a"},{3,"b"},{4,"b"},{5,"c"}} do
    rows[1+#rows] = cells(cols, r) end
  us = cols.x[1]:cut(rows)
  assert(#us == the.bins - 1, "Num cut = bins-1 boundaries")
  us = cols.x[2]:cut(rows)
  table.sort(us)
  assert(#us == 3 and us[1]=="a" and us[3]=="c", "Sym cut = uniques") end

eg["--divides"] = function(    cols, rows, ls, rs, lsum, rsum, op1, op2)
  cols = head{"X1","name"}; rows = {}
  for _,r in ipairs{{1,"a"},{2,"a"},{3,"b"},{4,"b"},{5,"c"}} do
    rows[1+#rows] = cells(cols, r) end
  ls, rs, lsum, rsum, op1, op2 =
    divides(cols.x[1], 2.5, rows, function(r) return r[1] end)
  assert(#ls == 2 and #rs == 3, "Num split at 2.5")
  assert(op1 == "<=" and op2 == ">", "Num ops")
  assert(lsum.has[1] == 1 and lsum.has[2] == 1, "left Sym counts 1,2")
  ls, rs, lsum, rsum, op1, op2 =
    divides(cols.x[2], "a", rows, function(r) return r[2] end)
  assert(#ls == 2 and #rs == 3, "Sym split eq a")
  assert(op1 == "==" and op2 == "~=", "Sym ops") end

eg["--cuts"] = function(    cols, rows, best, y)
  seed(the.seed)
  cols = head{"X1","X2","class!"}; rows = {}
  for _=1,40 do rows[1+#rows] =
    cells(cols, {rand(), rand(), rand() < 0.5 and "a" or "b"}) end
  y = function(r) return r[3] end
  best = cuts(cols, rows, y)
  assert(best and best.col and best.v, "cuts returns {col,v}") end

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
