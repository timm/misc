#!/usr/bin/env lua
-- ezr.lua : data-lite active learning.
local the, help = {}, [[
ezr.lua : explainable multi-objective optimization
(c) 2026, Tim Menzies <timm@ieee.org>, MIT license

USAGE
  lua ezr.lua --cmd [arg]   [-flag value]...

CMDS  (--all runs every --xxx command)
  --the     print config
  --csv     dump every 30th row of the CSV
  --data    build Data, show y-column mids
  --tree    random-label, grow tree, pretty-print
  --active  20 train/test splits; prints: train hold labels

OPTIONS
  -B Budget=50     label budget
  -C Check=5       final holdout check size
  -f few=128       max unlabelled pool
  -l leaf=3        min rows per tree leaf
  -p p=2           Minkowski distance coefficient
  -s seed=1        random seed
  -S Show=30       tree-display column width

CSV INPUT
  First row names columns; the suffix sets the role.
    [A-Z]*   numeric           [a-z]*   symbolic
    [A-Z]*-  minimize (y)      [A-Z]*+  maximize (y)
    *!       class (y)         *X       skip
    ?        missing value

NAMING
  i         self (in methods)     t     Tree node
  c         column (Num/Sym)      x     x-column (feature)
  r, rs     row, rows             y     y-column (goal)
  d         Data                  j     iterator var
  txt       string/text           fn    function
  _mid      cached centroid
]]

local l, eg = {}, {}
local NUM, SYM, COLS, DATA, TREE = {}, {}, {}, {}, {}
local Tree, Sym, Num, Cols, Data
local add, sub, adds, wins, split
local closer, rebalance, active, validate
local abs,min,max = math.abs,math.min,math.max
local log,exp = math.log,math.exp
local floor,rand,seedrng   = math.floor,math.random,math.randomseed

-- ## structs ---------------------------------------------------
function Tree(fn) return l.new(TREE,{score=fn}) end

function Sym(s,n)
  return l.new(SYM,{txt=s or "",at=n or 0,has={},n=0}) end

function Num(s,n)
  return l.new(NUM,{txt=s or "",at=n or 0,n=0,mu=0,m2=0,
                    heaven=s and s:match"-$" and 0 or 1}) end

function Cols(names,    xs,ys,all,col)
  xs,ys,all = {},{},{}
  for at,s in ipairs(names) do
    col = l.push(all,(s:match"^[A-Z]" and Num or Sym)(s,at))
    if not s:match"X$" then
      l.push(s:match"[%+%-!]$" and ys or xs, col) end end
  return l.new(COLS,{x=xs,y=ys,all=all,names=names}) end

function Data(src,    d)
  d = l.new(DATA,{rows={},cols=nil,_mid=nil})
  if type(src)=="string"
  then for   r in l.csv(src)        do add(d,r) end
  else for _,r in ipairs(src or {}) do add(d,r) end end
  return d end

function DATA.clone(i,rs)
  return adds(rs or {}, Data({i.cols.names})) end

-- ## update ----------------------------------------------------
function add(i,v,w) if v~="?" then i:_add(v,w or 1) end; return v end
function sub(i,v)   return i:_add(v,-1) end

function adds(vs,s)
  s = s or Num()
  for _,v in ipairs(vs or {}) do add(s,v) end
  return s end

function NUM._add(i,v,w,    d)
  i.n = i.n + w
  if w<0 and i.n<=1 then i.n,i.mu,i.m2 = 0,0,0
  elseif i.n > 0 then
    d = v-i.mu
    i.mu = i.mu + w*d/i.n
    i.m2 = i.m2 + w*d*(v-i.mu) end end

function SYM._add(i,v,w) i.has[v] = w + (i.has[v] or 0) end

function COLS._add(i,row,w)
  for _,c in ipairs(i.all) do add(c,row[c.at],w) end
  return row end

function DATA._add(i,row,w)
  if not i.cols then i.cols = Cols(row)
  else 
    i._mid = nil
    add(i.cols,row,w)
    if w>0 then l.push(i.rows,row)
    else
      for n,r in ipairs(i.rows) do
        if r.id==row.id
        then table.remove(i.rows,n); break end end end end end

-- ## stats -----------------------------------------------------
function NUM.mid(i) return i.mu end

function SYM.mid(i,    most,mode)
  most = -1
  for v,n in pairs(i.has) do if n>most then most,mode = n,v end end
  return mode end

function DATA.mid(i)
  i._mid = i._mid or l.map(i.cols.all,function(c) return c:mid() end)
  return i._mid end

function NUM.spread(i)
  return i.n>1 and (max(0,i.m2)/(i.n-1))^0.5 or 0 end

function SYM.spread(i)
  return -l.sum(i.has,function(v) return v/i.n*log(v/i.n,2) end) end

-- Sigmoid-of-z-score: approximates the Gaussian CDF in [0,1].
function NUM.norm(i,v,    z)
  if v=="?" then return v end
  z = (v - i.mu)/(i:spread() + 1e-32)
  return 1/(1 + exp(-1.7*l.crop(z,-3,3))) end

-- ## distance --------------------------------------------------
-- Per-column distance; when missing, assume far from the known side.
function NUM.aha(i,a,b)
  if a=="?" and b=="?" then return 1 end
  a,b = i:norm(a), i:norm(b)
  if a=="?" then a = b>0.5 and 0 or 1 end
  if b=="?" then b = a>0.5 and 0 or 1 end
  return abs(a-b) end

function SYM.aha(i,a,b)
  if a=="?" and b=="?" then return 1 end
  return a==b and 0 or 1 end

function DATA.disty(i,row,    fn)
  fn = function(c) return abs(c:norm(row[c.at]) - c.heaven)^the.p end
  return (l.sum(i.cols.y,fn)/#i.cols.y)^(1/the.p) end

function DATA.distx(i,r1,r2,    fn)
  fn = function(c) return c:aha(r1[c.at],r2[c.at])^the.p end
  return (l.sum(i.cols.x,fn)/#i.cols.x)^(1/the.p) end

-- ## tree ------------------------------------------------------
-- Greedy tree; stops at 2*leaf rows; picks min-weighted-spread split.
function TREE.build(i,d,rs,    mid,best,bw,w)
  mid    = d:clone(rs):mid()
  i.y    = adds(l.map(rs,function(r) return i.score(r) end))
  i.mids = l.kv(d.cols.y, function(c) return c.txt end,
                          function(c) return mid[c.at] end)
  if #rs < 2*the.leaf then return i end
  best, bw = nil, 1E32
  for _,c in ipairs(d.cols.x) do
    for _,sp in ipairs(c:splits(rs,i.score)) do
      w = sp.lhs.n*sp.lhs:spread() + sp.rhs.n*sp.rhs:spread()
      if w<bw and min(#sp.left,#sp.right)>=the.leaf then
        best, bw = sp, w end end end
  if best then
    i.col, i.cut, i.at = best.col, best.cut, best.col.at
    i.left  = Tree(i.score):build(d, best.left)
    i.right = Tree(i.score):build(d, best.right) end
  return i end

function TREE.leaf(i,row,    v,ok)
  if not i.col then return i end
  v = row[i.at]
  if v=="?" then return i.left:leaf(row) end
  ok = i.col.mu and (v<=i.cut) or (v==i.cut)
  return (ok and i.left or i.right):leaf(row) end

function TREE.nodes(i,fn,lvl,pre,    ys,ns,kids)
  lvl, pre = lvl or 0, pre or ""
  fn(i,lvl,pre)
  if not i.col then return end
  ys, ns = i.col.mu and "<=" or "==", i.col.mu and ">" or "!="
  kids = l.sort({{i.left,ys},{i.right,ns}},
                function(a,b) return a[1].y:mid() < b[1].y:mid() end)
  for _,p in ipairs(kids) do
    p[1]:nodes(fn,lvl+1,i.col.txt.." "..p[2].." "..l.o(i.cut)) end end

function TREE.show(i)
  i:nodes(function(n,lvl,pre,    s)
    s = lvl>0 and string.rep("|   ",lvl-1)..pre or ""
    io.write(l.fmt("%-"..the.Show.."s ,%5.2f ,(%3d),  %s\n",
             s, n.y:mid(), n.y.n, l.o(n.mids))) end) end

function split(col,rs,fn,cut,test,    lhs,rhs,L,R,ok)
  lhs,rhs,L,R = Num(),Num(),{},{}
  for _,r in ipairs(rs) do
    ok = r[col.at]=="?" or test(r[col.at])
    l.push(ok and L or R, r); add(ok and lhs or rhs, fn(r)) end
  if #L>=the.leaf and #R>=the.leaf then
    return {col=col,cut=cut,left=L,right=R,lhs=lhs,rhs=rhs} end end

-- Single cut at the median. Regularization: fewer cuts, less overfit.
function NUM.splits(i,rs,fn,    vs,mu,sp)
  vs = {}
  for _,r in ipairs(rs) do
    if r[i.at]~="?" then l.push(vs,r[i.at]) end end
  if #vs<2 then return {} end
  l.sort(vs)
  mu = vs[#vs//2+1]
  sp = split(i,rs,fn,mu,function(v) return v<=mu end)
  return sp and {sp} or {} end

function SYM.splits(i,rs,fn,    seen,out,sp)
  seen, out = {}, {}
  for _,r in ipairs(rs) do
    local v = r[i.at]
    if v~="?" and not seen[v] then
      seen[v] = true
      sp = split(i,rs,fn,v,function(x) return x==v end)
      if sp then l.push(out,sp) end end end
  return out end

-- ## active learning -------------------------------------------
-- Returns a closure scoring a row 0..100: 100=best disty, 50=median.
function wins(d,    ys,lo,md)
  ys = l.sort(l.map(d.rows,function(r) return d:disty(r) end))
  lo, md = ys[1], ys[#ys//2+1]
  return function(r)
    return floor(100*(1 - (d:disty(r)-lo)/(md-lo+1e-32))) end end

function closer(lab,best,rest,r)
  return lab:distx(r,best:mid()) < lab:distx(r,rest:mid()) end

function rebalance(best,rest,lab,    bad,badD,d)
  if #best.rows > (#lab.rows)^0.5 then
    badD = -1E32
    for _,r in ipairs(best.rows) do
      d = lab:disty(r)
      if d>badD then bad, badD = r, d end end
    sub(best,bad); add(rest,bad) end end

-- Active-label until budget spent. Returns (best, lab, labels_used).
function active(rs,data,    lab,best,rest,ys,sorted,n)
  lab, best, rest = data:clone(), data:clone(), data:clone()
  ys = Num()
  for k = 1, min(4,#rs) do
    add(lab,rs[k]); add(ys, lab:disty(rs[k])) end
  sorted = {}
  for _,r in ipairs(lab.rows) do l.push(sorted,r) end
  l.sort(sorted, function(a,b) return lab:disty(a) < lab:disty(b) end)
  n = max(1, floor((#sorted)^0.5))
  for k = 1,       n        do add(best, sorted[k]) end
  for k = n+1,     #sorted  do add(rest, sorted[k]) end
  for k = 5, min(the.few, #rs) do
    if ys.n >= the.Budget then break end
    if closer(lab,best,rest,rs[k]) then
      add(ys, lab:disty(rs[k]))
      add(lab, rs[k]); add(best, rs[k])
      rebalance(best,rest,lab) end end
  return best, lab, ys.n end

-- Split train/test; build tree on labels; pay CHECK on top picks.
function validate(rs,d,win)
  local n,train,test,_,lab,lbl,tb,tbD,di,tr,top
  l.shuffle(rs)
  n     = #rs // 2
  train = l.slice(rs, 1, n)
  test  = l.slice(rs, n+1)
  _, lab, lbl = active(train, d)
  tb, tbD = nil, 1E32
  for _,r in ipairs(lab.rows) do
    di = d:disty(r)
    if di<tbD then tb, tbD = r, di end end
  tr = Tree(function(r) return lab:disty(r) end):build(lab,lab.rows)
  l.sort(test, function(a,b)
    return tr:leaf(a).y:mid() < tr:leaf(b).y:mid() end)
  top = l.sort(l.slice(test,1,the.Check),
               function(a,b) return d:disty(a) < d:disty(b) end)
  return win(tb), win(top[1]), lbl + the.Check end

-- ## lib -------------------------------------------------------
function l.new(kl,obj) 
  kl.__index = kl; return setmetatable(obj,kl) end

function l.crop(n,lo,hi) return max(lo,min(hi,n)) end

function l.push(t,x) t[1+#t] = x; return x end

function l.sort(t,f) table.sort(t,f); return t end

function l.sum(t,f,    n)
  n=0; for _,x in pairs(t) do n=n+f(x) end; return n end

function l.map(t,f,    u)
  u={}; for i,x in ipairs(t) do u[i]=f(x) end; return u end

function l.kv(t,fk,fv,    u)
  u={}; for _,x in ipairs(t) do u[fk(x)]=fv(x) end; return u end

function l.slice(t,lo,hi,    u)
  u={}; for i=(lo or 1),(hi or #t) do u[1+#u]=t[i] end; return u end

function l.shuffle(t,    j)
  for i=#t,2,-1 do j=rand(i); t[i],t[j]=t[j],t[i] end; return t end

function l.many(t,n) return l.slice(l.shuffle(t),1,n) end

l.fmt = string.format

function l.o(x,    u)
  if type(x) ~= "table" then
    return math.type(x)=="float" and l.fmt("%.2f",x) or tostring(x) end
  u = {}
  for k,v in pairs(x) do
    u[1+#u] = type(k)=="number" and l.o(v) or k.."="..l.o(v) end
  return "{"..table.concat(l.sort(u),", ").."}" end

-- Coerce string to boolean, number, or keep as string.
function l.thing(s)
  return s=="true" or (s~="false" and (tonumber(s) or s)) end

-- Iterator yielding typed rows from a CSV file.
function l.csv(src,    f,id)
  f = assert(io.open(src), "cannot open "..tostring(src))
  id = 0
  return function(    s,t)
    s = f:read()
    if s then
      t = {}; id = id + 1; t.id = id
      for x in s:gmatch"[^,]+" do
        l.push(t, l.thing(x:match"^%s*(.-)%s*$")) end
      return t
    else f:close() end end end

-- ## eg --------------------------------------------------------
eg["-h"]     = function(_) print(help) end

eg["--the"]  = function(_) print(l.o(the)) end

eg["--all"]  = function(arg,    ss)
  ss = {}
  for k in pairs(eg) do if k~="--all" then l.push(ss,k) end end
  for _,k in ipairs(l.sort(ss)) do
    print("\n"..k); seedrng(the.seed); eg[k](arg) end end

eg["--csv"]  = function(f,    n)
  n=0; for r in l.csv(f) do
    if n%30==0 then print(l.o(r)) end; n=n+1 end end

eg["--data"] = function(f,    d)
  d = Data(f)
  for _,c in ipairs(d.cols.y) do print(c.txt, l.o(c:mid())) end end

eg["--tree"] = function(f,    d,rs)
  d  = Data(f)
  rs = l.many(d.rows, the.Budget)
  d  = d:clone(rs)
  Tree(function(r) return d:disty(r) end):build(d, d.rows):show() end

eg["--active"] = function(src,    d,win,tt,hh,nn)
  d = Data(src); win = wins(d)
  for _ = 1, 20 do
    tt, hh, nn = validate(d.rows, d, win)
    io.write(l.fmt("%3d %3d %3d\n", tt, hh, nn)) end end

-- ## main ------------------------------------------------------
for k,v in help:gmatch("%-%S%s+(%w+)=(%S+)") do
  the[k] = l.thing(v) end

local function main(    n,k,v)
  n = 1
  while n <= #arg do
    n, k, v = n+1, arg[n], arg[n+1]
    if eg[k] then
      seedrng(the.seed)
      eg[k](v and l.thing(v) or nil)
      if v and not eg[v] then n = n+1 end
    else
      for k1 in pairs(the) do
        if k == "-"..k1:sub(1,1) then
          the[k1] = l.thing(v); n = n+1 end end end end end

if (arg[0] or ""):match"ezr.lua" then main() end

return {the=the,DATA=DATA,NUM=NUM,SYM=SYM,TREE=TREE,
        l=l,active=active,validate=validate}
