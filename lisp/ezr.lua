#!/usr/bin/env lua
-- ezr.lua : data-lite active learning.
local the, help = {}, [[
ezr.lua : explainable multi-objective optimization
(c) 2026, Tim Menzies <timm@ieee.org>, MIT license

USAGE
  lua ezr.lua --cmd [arg]   [-flag value]...

CMDS  (--all runs every --xxx command)
  --the       print config
  --csv       dump every 30th row of the CSV
  --data      build Data, show y-column mids
  --tree      random-label, grow tree, pretty-print (== --see)
  --see       Rung 1: show the tree
  --act       Rung 2: tree leaves vs reality; flag surprises
  --imagine   Rung 3: leaf-to-leaf counterfactual plans
  --active    20 train/test splits; prints: train hold labels
  --classify  incremental Naive Bayes (needs a *! class column)
  --bayes     run classify on canonical soybean and diabetes
  --which     grow rule ranges separating best from rest
  --sa        simulated annealing search
  --ls        local search
  --compare   sa vs ls vs random over 20 runs; print bestRanks
  --cluster   k-means (5 clusters) + kpp seed summary
  --check     sanity-test the library helpers (bchop, argmin, pick, merge)

OPTIONS
  -B Budget=50     label budget
  -b bins=16       discretization bin count
  -C Check=5       final holdout check size
  -E Evals=500     oracle-call budget for sa/ls/compare
  -f few=128       max unlabelled pool
  -G gens=20       WHICH generations
  -g gap=0.35      plan-effect threshold
  -k k=1           Bayes Laplace smoothing
  -l leaf=3        min rows per tree leaf
  -m m=2           Bayes m-estimate
  -p p=2           Minkowski distance coefficient
  -s seed=1        random seed
  -S Show=30       tree-display column width
  -W cap=32        WHICH stack cap

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
local closer, rebalance, active, validate, acquireBayes
local abs,min,max = math.abs,math.min,math.max
local log,exp,pi = math.log,math.exp,math.pi
local floor,sqrt = math.floor,math.sqrt
local rand,seedrng = math.random,math.randomseed

-- ## structs ---------------------------------------------------
function Tree(fn) return l.new(TREE,{score=fn}) end

function Sym(s,n)
  return l.new(SYM,{txt=s or "",at=n or 0,has={},n=0}) end

function Num(s,n)
  return l.new(NUM,{txt=s or "",at=n or 0,n=0,mu=0,m2=0,
                    heaven=s and s:match"-$" and 0 or 1}) end

function Cols(names,    xs,ys,all,col,klass)
  xs,ys,all,klass = {},{},{},nil
  for at,s in ipairs(names) do
    col = l.push(all,(s:match"^[A-Z]" and Num or Sym)(s,at))
    if s:match"!$" then klass = col end
    if not s:match"X$" then
      l.push(s:match"[%+%-!]$" and ys or xs, col) end end
  return l.new(COLS,{x=xs,y=ys,all=all,klass=klass,names=names}) end

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
function sub(i,v)   return add(i,v,-1) end

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

function SYM._add(i,v,w)
  i.has[v] = w + (i.has[v] or 0); i.n = i.n + w end

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

-- Irwin-Hall (sum of 3 uniforms, centered) gives a cheap Gaussian around v
-- (or mu if v is nil/missing), clamped to mu +/- 3*sd.
function NUM.pick(i,v,    sd,lo,hi,new)
  sd = i:spread()
  lo, hi = i.mu - 3*sd, i.mu + 3*sd
  new = ((v and v~="?") and v or i.mu)
        + sd*2*(rand()+rand()+rand() - 1.5)
  return lo + (new - lo) % (hi - lo + 1e-32) end

-- Bernoulli sample from observed symbols, weighted by frequency.
function SYM.pick(i,    r,v,c)
  r = rand() * i.n
  for v,c in pairs(i.has) do
    r = r - c; if r <= 0 then return v end end end

-- Does col's value v lie in range rng?  Dispatched by col type.
function NUM.covers(i,rng,v)
  return v ~= "?" and v >= rng.lo and v <= rng.hi end

function SYM.covers(i,rng,v)
  return v ~= "?" and v == rng.v end

-- Merge two column summaries. NUM uses Welford-parallel combine.
function NUM.merge(i,j,    s,d,n)
  s, n = Num(), i.n + j.n
  if n == 0 then return s end
  d = j.mu - i.mu
  s.n, s.mu = n, i.mu + d*j.n/n
  s.m2 = i.m2 + j.m2 + d*d*i.n*j.n/n
  return s end

function SYM.merge(i,j,    s,v,n)
  s = Sym()
  for v,n in pairs(i.has) do s.has[v] = n; s.n = s.n + n end
  for v,n in pairs(j.has) do
    s.has[v] = (s.has[v] or 0) + n; s.n = s.n + n end
  return s end

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

-- Minkowski aggregate with exponent the.p. Caller supplies fn returning the
-- raw per-element distance; minkowski raises to p, averages, roots.
function l.minkowski(t,fn,    pw)
  pw = function(c) return fn(c)^the.p end
  return (l.sum(t,pw)/#t)^(1/the.p) end

function DATA.disty(i,row)
  return l.minkowski(i.cols.y, function(c)
    return abs(c:norm(row[c.at]) - c.heaven) end) end

function DATA.distx(i,r1,r2)
  return l.minkowski(i.cols.x, function(c)
    return c:aha(r1[c.at],r2[c.at]) end) end

-- ## bayes -----------------------------------------------------
-- Column-local likelihood with Laplace (Sym) / Gaussian (Num).
function l.like(c,v,prior,    sd)
  if v=="?" then return 1 end
  if c.has then
    return ((c.has[v] or 0) + the.k*prior)/(c.n + the.k) end
  sd = c:spread() + 1e-32
  return (1/(2*pi*sd*sd)^0.5) * exp(-(v-c.mu)^2/(2*sd*sd)) end

-- Log-likelihood of row r under Data d, given global nall and nklass.
function l.likes(d,r,nall,nh,    prior,out,v,p)
  prior = (#d.rows + the.m)/(nall + the.m*nh)
  out = log(prior)
  for _,c in ipairs(d.cols.x) do
    v = r[c.at]
    if v ~= "?" then
      p = l.like(c,v,prior)
      if p > 0 then out = out + log(p) end end end
  return out end

-- Test-then-train classify (incremental Naive Bayes). Returns nil if the
-- CSV has no `!` class column.
function l.classify(src,wait,    h,cf,all,n,predict,tally,want,r)
  h, cf, wait, n = {}, {}, wait or 10, 0
  predict = function(r,   best,bestV,nh,kl,d,v)
    nh, bestV = 0, -1E32
    for _ in pairs(h) do nh = nh + 1 end
    for kl,d in pairs(h) do
      v = l.likes(d, r, #all.rows, nh)
      if v > bestV then bestV, best = v, kl end end
    return best end
  tally = function(want,got)
    cf[want] = cf[want] or {}
    cf[want][got] = (cf[want][got] or 0) + 1 end
  for r in l.csv(src) do
    if not all then
      all = Data({r})
      if not all.cols.klass then return nil end
    else
      want = r[all.cols.klass.at]
      if n >= wait then tally(want, predict(r)) end
      h[want] = h[want] or all:clone()
      add(h[want], r); add(all, r); n = n + 1 end end
  return cf end

function l.confused(cf,    labels,total,p,stats,tot)
  labels = function(   seen,out,w,row,g)
    seen, out = {}, {}
    for w,row in pairs(cf) do
      if not seen[w] then l.push(out,w); seen[w]=true end
      for g in pairs(row) do
        if not seen[g] then l.push(out,g); seen[g]=true end end end
    return l.sort(out, function(a,b) return tostring(a) < tostring(b) end) end
  total = function(   t,w,row,c)
    t = 0
    for w,row in pairs(cf) do for _,c in pairs(row) do t = t + c end end
    return t end
  p = function(y,z) return floor(100*y/(z==0 and 1e-32 or z)) end
  stats = function(c,    tp,fn1,fp,tn,pd,pr,sp,w,row,g)
    tp = (cf[c] or {})[c] or 0
    fn1, fp = 0, 0
    for _,g in pairs(cf[c] or {}) do fn1 = fn1 + g end
    fn1 = fn1 - tp
    for w,row in pairs(cf) do
      if w~=c then fp = fp + (row[c] or 0) end end
    tn = tot - tp - fn1 - fp
    pd, pr, sp = p(tp,tp+fn1), p(tp,fp+tp), p(tn,tn+fp)
    return {lab=c, tp=tp, fn=fn1, fp=fp, tn=tn,
            pd=pd, pr=pr, acc=p(tp+tn,tot),
            f1=floor(2*pd*pr/(pd+pr+1e-32)),
            g =floor(2*pd*sp/(pd+sp+1e-32))} end
  tot = total()
  return l.map(labels(), stats) end

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

-- Rung 3: counterfactual leaves that beat `here` by > gap*spread.
function TREE.plan(i,here,    out,eps,dy,diff)
  out, eps = {}, the.gap * here.y:spread()
  i:nodes(function(n)
    if not n.col and n ~= here then
      dy = here.y:mid() - n.y:mid()
      if dy > eps then
        diff = {}
        for k,v in pairs(n.mids) do
          if here.mids[k] ~= v then
            l.push(diff, k.."="..l.o(v)) end end
        if #diff > 0 then
          l.push(out,{dy=dy, score=n.y:mid(), diff=diff}) end end end end)
  return l.sort(out, function(a,b) return a.dy > b.dy end) end

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

function SYM.splits(i,rs,fn,    seen,out,sp,v)
  seen, out = {}, {}
  for _,r in ipairs(rs) do
    v = r[i.at]
    if v~="?" and not seen[v] then
      seen[v] = true
      sp = split(i,rs,fn,v,function(x) return x==v end)
      if sp then l.push(out,sp) end end end
  return out end

-- ## discretize ------------------------------------------------
-- Unified bin id: NUM => int via sigmoid-z; SYM => value itself.
function l.bin(c,v)
  if v=="?" then return v end
  if c.mu then return floor(the.bins * c:norm(v)) end
  return v end

-- Walk sorted (x,y) pairs; open a new bin once size>=big and span>iota.
-- makeY() builds the per-bin y-summary: Sym() for symbolic, Num() for numeric.
function l.unsuper(xys,big,iota,makeY,    now,all,x,y)
  makeY = makeY or function() return Sym() end
  l.sort(xys, function(a,b) return a[1]<b[1] end)
  now = {lo=xys[1][1],hi=xys[1][1],n=0,y=makeY()}
  all = {now}
  for i,xy in ipairs(xys) do
    x, y = xy[1], xy[2]
    if i < #xys - big and now.n >= big
       and (now.hi - now.lo) > iota and x ~= xys[i+1][1] then
      now = {lo=x,hi=x,n=0,y=makeY()}
      l.push(all, now) end
    now.n = now.n + 1
    now.hi = x
    add(now.y, y) end
  return all end

-- Merge adjacent bins bottom-up if combined y-spread does not grow.
-- Polymorphic: a.y:merge(b.y) dispatches to NUM.merge or SYM.merge.
function l.merge(b4,    tmp,j,a,b,cy)
  tmp, j = {}, 1
  while j <= #b4 do
    a = b4[j]
    if j < #b4 then
      b = b4[j+1]
      cy = a.y:merge(b.y)
      if cy:spread()*0.95 <=
         (a.y:spread()*a.n + b.y:spread()*b.n)/(a.n + b.n) then
        a = {lo=a.lo, hi=b.hi, n=a.n+b.n, y=cy}
        j = j + 1 end end
    l.push(tmp,a); j = j + 1 end
  return #tmp < #b4 and l.merge(tmp) or b4 end

-- ## rules (WHICH) ---------------------------------------------
-- Range covers r iff rng.col's method says so.
function l.cover(r,rng) return rng.col:covers(rng, r[rng.col.at]) end

function l.allCover(r,combo)
  for _,rng in ipairs(combo) do
    if not l.cover(r,rng) then return false end end
  return true end

-- Count rows in t whose values satisfy combo.
function l.tally(t,combo,    n)
  n = 0; for _,r in ipairs(t) do
    if l.allCover(r,combo) then n = n + 1 end end
  return n end

-- b^2/(b+r): support * P(best|covered).
function l.whichScore(combo,best,rest,    b,r)
  b, r = l.tally(best,combo), l.tally(rest,combo)
  return b*b/(b + r + 1e-32) end

-- Seed ranges per column: unsuper+merge (Num) or per-value (Sym).
function l.seedRanges(best,rest,cols,    out,xys,rngs)
  out = {}
  for _,c in ipairs(cols) do
    if c.mu then
      xys = {}
      for _,r in ipairs(best.rows) do
        if r[c.at]~="?" then l.push(xys,{r[c.at],"best"}) end end
      for _,r in ipairs(rest.rows) do
        if r[c.at]~="?" then l.push(xys,{r[c.at],"rest"}) end end
      if #xys > 0 then
        rngs = l.merge(l.unsuper(xys, #xys//8+1, c:spread()/8))
        for _,bin in ipairs(rngs) do
          l.push(out,{col=c,lo=bin.lo,hi=bin.hi}) end end
    else
      for v in pairs(c.has) do l.push(out,{col=c,v=v}) end end end
  return out end

-- WHICH: grow stack of range-conjunctions via rank-weighted mating.
function l.which(best,rest,cols,    byScore,stack,a,b,kid,s,rng)
  byScore = function(a,b) return a.s > b.s end
  stack = {}
  for _,rng in ipairs(l.seedRanges(best,rest,cols)) do
    l.push(stack,{combo={rng},
                  s=l.whichScore({rng},best.rows,rest.rows)}) end
  l.sort(stack, byScore)
  for _ = 1, the.gens do
    a, b = l.pickRank(stack), l.pickRank(stack)
    kid = {}
    for _,rng in ipairs(a.combo) do l.push(kid,rng) end
    for _,rng in ipairs(b.combo) do l.push(kid,rng) end
    s = l.whichScore(kid,best.rows,rest.rows)
    l.push(stack,{combo=kid,s=s})
    l.sort(stack, byScore)
    while #stack > the.cap do stack[#stack] = nil end end
  return stack[1] end

-- ## active learning -------------------------------------------
-- Returns a closure scoring a row 0..100: 100=best disty, 50=median.
function wins(d,    ys,lo,md)
  ys = l.sort(l.map(d.rows,function(r) return d:disty(r) end))
  lo, md = ys[1], ys[#ys//2+1]
  return function(r)
    return floor(100*(1 - (d:disty(r)-lo)/(md-lo+1e-32))) end end

-- Default acquirer: is r closer to best-centroid than rest-centroid?
function closer(lab,best,rest,r,t)
  return lab:distx(r,best:mid()) < lab:distx(r,rest:mid()) end

-- Bayes acquirer: is r likelier under best than under rest?
function acquireBayes(lab,best,rest,r,t,    nall)
  nall = #best.rows + #rest.rows
  return l.likes(best,r,nall,2) > l.likes(rest,r,nall,2) end

function rebalance(best,rest,lab,    bad)
  if #best.rows > (#lab.rows)^0.5 then
    bad = l.argmax(best.rows, function(r) return lab:disty(r) end)
    add(rest, sub(best,bad)) end end

-- Active-label until budget. score(lab,best,rest,r,t)=>bool; t=progress.
function active(rs,data,score,    lab,best,rest,ys,sorted,n,t)
  score = score or closer
  lab, best, rest = data:clone(), data:clone(), data:clone()
  ys = Num()
  for k = 1, min(4,#rs) do
    add(lab,rs[k]); add(ys, lab:disty(rs[k])) end
  sorted = {}
  for _,r in ipairs(lab.rows) do l.push(sorted,r) end
  l.sort(sorted, function(a,b) return lab:disty(a) < lab:disty(b) end)
  n = max(1, floor((#sorted)^0.5))
  for k = 1,   n        do add(best, sorted[k]) end
  for k = n+1, #sorted  do add(rest, sorted[k]) end
  for k = 5, min(the.few, #rs) do
    if ys.n >= the.Budget then break end
    t = ys.n / the.Budget
    if score(lab,best,rest,rs[k],t) then
      add(ys, lab:disty(rs[k]))
      add(lab, rs[k]); add(best, rs[k])
      rebalance(best,rest,lab) end end
  return best, lab, ys.n end

-- Split train/test; build tree on labels; pay CHECK on top picks.
function validate(rs,d,win,    n,train,test,_,lab,lbl,tb,tr,top)
  l.shuffle(rs)
  n     = #rs // 2
  train = l.slice(rs, 1, n)
  test  = l.slice(rs, n+1)
  _, lab, lbl = active(train, d)
  tb = l.argmin(lab.rows, function(r) return d:disty(r) end)
  tr = Tree(function(r) return lab:disty(r) end):build(lab,lab.rows)
  l.sort(test, function(a,b)
    return tr:leaf(a).y:mid() < tr:leaf(b).y:mid() end)
  top = l.sort(l.slice(test,1,the.Check),
               function(a,b) return d:disty(a) < d:disty(b) end)
  return win(tb), win(top[1]), lbl + the.Check end

-- ## search (1+1) ----------------------------------------------
-- Mutate n x-cols of r via polymorphic column pick.
function l.picks(d,r,n,    s)
  s = {}; for i,x in ipairs(r) do s[i] = x end
  for _,c in ipairs(l.many(d.cols.x, min(n,#d.cols.x))) do
    s[c.at] = c:pick(s[c.at]) end
  return s end

-- Nearest-neighbor surrogate: find closest known row on x; copy its y-cols
-- into r; return r's disty under `known`. Makes the oracle depend on x.
function l.oracleNearest(known,r,    best)
  best = l.argmin(known.rows, function(kr) return known:distx(r,kr) end)
  for _,c in ipairs(known.cols.y) do r[c.at] = best[c.at] end
  return known:disty(r) end

-- Copy one random row from d (fresh mutation seed / restart point).
local function seed(d,    s)
  s = {}; for i,x in ipairs(l.many(d.rows,1)[1]) do s[i] = x end
  return s end

-- Generic (1+1): mutate, score, accept. `mutate(s)` returns a list of
-- candidates to try in sequence. `accept(e,en,h,budget)` returns bool.
function l.oneplus1(d,mutate,accept,oracle,budget,restarts,
                    h,best,bestE,s,e,imp,sn,en)
  h, best, bestE = 0, nil, 1E32
  s, e, imp = seed(d), 1E32, 0
  while h < budget do
    for _,sn in ipairs(mutate(s)) do
      h = h + 1; en = oracle(sn)
      if accept(e,en,h,budget) then s, e = sn, en end
      if en < bestE then best, bestE, imp = sn, en, h end
      if h >= budget then break end end
    if restarts > 0 and h - imp > restarts then
      s, e, imp = seed(d), 1E32, h end end
  return best, bestE end

-- SA: stochastic accept (Boltzmann), mutate m*|xs| cols at once.
function l.sa(d,oracle,budget,restarts,m,    nmut)
  budget, restarts, m = budget or 1000, restarts or 0, m or 0.5
  nmut = max(1, floor(m * #d.cols.x))
  return l.oneplus1(d,
    function(s) return {l.picks(d,s,nmut)} end,
    function(e,en,h,b)
      return en<e or rand() < exp((e-en)/(1-h/b+1e-32)) end,
    oracle, budget, restarts) end

-- LS: greedy accept; pick ONE column, try up to `tries` variants of it.
function l.ls(d,oracle,budget,restarts,tries,p)
  budget, restarts = budget or 1000, restarts or 100
  tries, p = tries or 20, p or 0.5
  return l.oneplus1(d,
    function(s,   c,n,out,sn)
      c, n = d.cols.x[1+floor(rand()*#d.cols.x)],
             rand()<p and tries or 1
      out = {}
      for _ = 1, n do
        sn = {}; for i,x in ipairs(s) do sn[i] = x end
        sn[c.at] = c:pick(s[c.at]); l.push(out, sn) end
      return out end,
    function(e,en) return en < e end,
    oracle, budget, restarts) end

-- ## clustering ------------------------------------------------
-- Assign rows to nearest centroid; recompute centroid as mid; iterate.
function l.kmeans(d,rs,k,iters,cents,    out,best,bestD,dd,j,kid)
  rs, k, iters = rs or d.rows, k or 10, iters or 10
  cents = cents or l.many(rs, k)
  for _ = 1, iters do
    out = {}; for _ = 1, #cents do l.push(out, d:clone()) end
    for _,r in ipairs(rs) do
      best, bestD = 1, d:distx(cents[1], r)
      for j = 2, #cents do
        dd = d:distx(cents[j], r)
        if dd < bestD then best, bestD = j, dd end end
      add(out[best], r) end
    cents = {}
    for _,kid in ipairs(out) do
      if #kid.rows > 0 then l.push(cents, kid:mid()) end end end
  return out end

-- k-means++: next centroid sampled by min-squared-distance weight.
function l.kpp(d,rs,k,few,    out,t,ws,best,dd)
  rs, k, few = rs or d.rows, k or 10, few or 256
  out = {l.many(rs,1)[1]}
  while #out < k do
    t = l.many(rs, min(few, #rs))
    ws = {}
    for i = 1, #t do
      best = 1E32
      for _,c in ipairs(out) do
        dd = d:distx(t[i], c)^2
        if dd < best then best = dd end end
      ws[i] = best end
    l.push(out, t[l.pick(ws)]) end
  return out end

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

-- Traverse t in order: ipairs for arrays (#t>0), key-sorted otherwise.
-- Use: for k,v in l.items(t) do ... end
function l.items(t,    keys,i)
  if #t > 0 then return ipairs(t) end
  keys = {}
  for k in pairs(t) do l.push(keys,k) end
  l.sort(keys, function(a,b) return tostring(a) < tostring(b) end)
  i = 0
  return function()
    i = i + 1
    if keys[i] then return keys[i], t[keys[i]] end end end

-- Rank-weighted pick from sorted-descending list: early = likelier.
function l.pickRank(t,    r,w,acc)
  w = 0; for i=1,#t do w = w + 1/i end
  r = rand()*w; acc = 0
  for i=1,#t do
    acc = acc + 1/i
    if acc >= r then return t[i] end end
  return t[#t] end

-- Weighted-dict pick: sample a key proportional to its value.
function l.pick(ws,    sum,r,k,w)
  sum = 0; for _,w in pairs(ws) do sum = sum + w end
  r = rand() * sum
  for k,w in pairs(ws) do
    r = r - w
    if r <= 0 then return k end end end

-- Binary search. Returns count of elements <= v; or < v if strict=true.
function l.bchop(sorted,v,strict,    lo,hi,mid,x)
  lo, hi = 1, #sorted + 1
  while lo < hi do
    mid = (lo + hi) // 2; x = sorted[mid]
    if strict and x >= v or not strict and x > v then hi = mid
    else lo = mid + 1 end end
  return lo - 1 end

-- Argmin/argmax over t by fn(x). Return (x, fn(x)).
function l.argmin(t,fn,    best,bestV,v)
  bestV = 1E32
  for _,x in ipairs(t) do
    v = fn(x); if v < bestV then best, bestV = x, v end end
  return best, bestV end

function l.argmax(t,fn,    best,bestV,v)
  bestV = -1E32
  for _,x in ipairs(t) do
    v = fn(x); if v > bestV then best, bestV = x, v end end
  return best, bestV end

l.fmt = string.format

function l.o(x,    u)
  if type(x) ~= "table" then
    return math.type(x)=="float" and l.fmt("%.2f",x) or tostring(x) end
  u = {}
  for k,v in l.items(x) do
    u[1+#u] = type(k)=="number" and l.o(v) or k.."="..l.o(v) end
  return "{"..table.concat(u,", ").."}" end

-- Aligned table printer. rows = list of dicts; keys = ordered column names.
-- Header and separator lines start with "# " (grep-friendly: strip comments
-- with `grep -v '^#'`, then sort/awk/filter the data).  Data rows start with
-- "  " so columns stay aligned with the header.  Numeric cols right-align to
-- max width; the last key left-aligns so long labels extend freely.
function l.table(rows,keys,    wmap,pad,last,s,w,j,k,row)
  if #rows == 0 then return end
  last = keys[#keys]
  wmap = {}
  for _,k in ipairs(keys) do wmap[k] = #tostring(k) end
  for _,row in ipairs(rows) do
    for _,k in ipairs(keys) do
      s = l.o(row[k])
      if #s > wmap[k] then wmap[k] = #s end end end
  pad = function(s,w) return string.rep(" ",w-#s)..s end
  io.write("# ")
  for j,k in ipairs(keys) do
    io.write((j>1 and "  " or "")
             ..(k==last and tostring(k) or pad(tostring(k),wmap[k]))) end
  io.write("\n# ")
  for j,k in ipairs(keys) do
    w = k==last and #tostring(k) or wmap[k]
    io.write((j>1 and "  " or "")..string.rep("-",w)) end
  io.write("\n")
  for _,row in ipairs(rows) do
    io.write("  ")
    for j,k in ipairs(keys) do
      s = l.o(row[k])
      io.write((j>1 and "  " or "")..(k==last and s or pad(s,wmap[k]))) end
    io.write("\n") end end

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

-- ## stats-compare ---------------------------------------------
-- Cliff's Delta + 2-sample KS via bchop: O((n+m) log (n+m)).
function l.same(xs,ys,eps,    n,m,gt,lt,ks,d,ksAt)
  xs, ys = l.sort(l.slice(xs)), l.sort(l.slice(ys))
  n, m = #xs, #ys
  if n==0 or m==0 then return false end
  if abs(xs[n//2+1] - ys[m//2+1]) <= eps then return true end
  gt, lt = 0, 0
  for _,a in ipairs(xs) do
    gt = gt + l.bchop(ys, a, true)        -- count of ys strictly < a
    lt = lt + (m - l.bchop(ys, a, false)) -- count of ys strictly > a
  end
  if abs(gt - lt)/(n*m) > 0.195 then return false end
  ks, ksAt = 0, function(v,  fa,fb)
    fa, fb = l.bchop(xs,v,false), l.bchop(ys,v,false)
    d = abs(fa/n - fb/m); if d > ks then ks = d end end
  for _,v in ipairs(xs) do ksAt(v) end
  for _,v in ipairs(ys) do ksAt(v) end
  return ks <= 1.36 * sqrt((n+m)/(n*m)) end

-- Group {name=list} by ties-for-best (after sorting by median ascending).
function l.bestRanks(d,eps,    items,out,lst0,aa,bb)
  items = {}
  for k,lst in pairs(d) do l.push(items,{k=k,lst=lst}) end
  l.sort(items, function(a,b)
    aa, bb = l.sort(l.slice(a.lst)), l.sort(l.slice(b.lst))
    return aa[#aa//2+1] < bb[#bb//2+1] end)
  lst0 = items[1].lst
  out = {[items[1].k] = lst0}
  for j = 2, #items do
    if l.same(lst0, items[j].lst, eps) then
      out[items[j].k] = items[j].lst
    else break end end
  return out end

-- ## eg --------------------------------------------------------
eg["-h"]     = function(_) print(help) end

eg["--the"]  = function(_) print(l.o(the)) end

eg["--all"]  = function(arg)
  arg = arg or "auto93.csv"
  for k,fn in l.items(eg) do
    if k ~= "--all" then
      print("\n"..k); seedrng(the.seed); fn(arg) end end end

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

eg["--see"] = eg["--tree"]

-- Rung 2: run rows down the tree; flag leaves that disagree with reality.
eg["--act"] = function(f,    d,rs,dt,t,sorted,lf,ac,pr,gap,flag,r)
  d  = Data(f); rs = l.many(d.rows, the.Budget)
  dt = d:clone(rs)
  t  = Tree(function(r) return dt:disty(r) end):build(dt, dt.rows)
  sorted = l.sort(l.slice(d.rows),
    function(a,b) return dt:disty(a) < dt:disty(b) end)
  for k = 1, min(10,#sorted) do
    r = sorted[k]
    lf = t:leaf(r); ac = dt:disty(r); pr = lf.y:mid()
    gap = ac - pr
    flag = abs(gap) > lf.y:spread() and " !" or "  "
    io.write(l.fmt("%s actual=%5.2f  leaf=%5.2f  gap=%6.2f  n=%d\n",
             flag, ac, pr, gap, lf.y.n)) end end

-- Rung 3: counterfactual plans from the worst row's leaf.
eg["--imagine"] = function(f,    d,rs,dt,t,here,worst)
  d  = Data(f); rs = l.many(d.rows, the.Budget)
  dt = d:clone(rs)
  t  = Tree(function(r) return dt:disty(r) end):build(dt, dt.rows)
  worst = l.argmax(d.rows, function(r) return d:disty(r) end)
  here = t:leaf(worst)
  print(l.fmt("now=%.2f", here.y:mid()))
  for _,p in ipairs(t:plan(here)) do
    print(l.fmt("  %5.2f (dy=%.2f) if %s",
                p.score, p.dy, table.concat(p.diff, ", "))) end end

eg["--classify"] = function(f,    cf)
  cf = l.classify(f)
  if not cf then print("--classify skipped: no !-class column"); return end
  l.table(l.confused(cf),
          {"pd","pr","f1","g","acc","tp","fn","fp","tn","lab"}) end

-- Run Naive Bayes on the canonical soybean and diabetes datasets
-- (expected in the current directory).
eg["--bayes"] = function(_,    f)
  for _,p in ipairs{"soybean.csv","diabetes.csv"} do
    f = io.open(p,"r")
    if f then f:close(); print("\n# "..p); eg["--classify"](p)
    else print("\n# missing: "..p) end end end

eg["--which"] = function(f,    d,n,bests,rests,top,parts,rng)
  d = Data(f)
  l.sort(d.rows, function(a,b) return d:disty(a) < d:disty(b) end)
  n = floor((#d.rows)^0.5)
  bests = d:clone(l.slice(d.rows, 1, n))
  rests = d:clone(l.slice(d.rows, n+1))
  top = l.which(bests, rests, d.cols.x)
  if top then
    parts = {}
    for _,rng in ipairs(top.combo) do
      l.push(parts, rng.v and rng.col.txt.."=="..l.o(rng.v)
              or l.fmt("%s in [%.2f,%.2f]",rng.col.txt,rng.lo,rng.hi)) end
    print(l.fmt("score=%.3f  rule: %s", top.s,
                table.concat(parts, " AND "))) end end

eg["--active"] = function(src,    d,win,tt,hh,nn)
  d = Data(src); win = wins(d)
  for _ = 1, 20 do
    tt, hh, nn = validate(d.rows, d, win)
    io.write(l.fmt("%3d %3d %3d\n", tt, hh, nn)) end end

eg["--sa"] = function(f,    d,known,oracle,row,_)
  d = Data(f); l.shuffle(d.rows)
  known = d:clone(l.slice(d.rows,1,50))
  oracle = function(r) return l.oracleNearest(known,r) end
  row, _ = l.sa(d, oracle, the.Evals, 100)
  print(l.fmt("sa  disty=%.3f  evals=%d", d:disty(row), the.Evals)) end

eg["--ls"] = function(f,    d,known,oracle,row,_)
  d = Data(f); l.shuffle(d.rows)
  known = d:clone(l.slice(d.rows,1,50))
  oracle = function(r) return l.oracleNearest(known,r) end
  row, _ = l.ls(d, oracle, the.Evals, 100)
  print(l.fmt("ls  disty=%.3f  evals=%d", d:disty(row), the.Evals)) end

-- 4 search variants + random baseline. Shows restarts dominate mutation.
-- 5-cluster k-means then 5-centroid k-means++ seed on the given CSV.
eg["--cluster"] = function(f,    d,km,cs,kid,mu)
  d = Data(f)
  km = l.kmeans(d, d.rows, 5, 5)
  print("# k-means (5 clusters, 5 iters):")
  for i,kid in ipairs(km) do
    mu = 0
    for _,r in ipairs(kid.rows) do mu = mu + kid:disty(r) end
    print(l.fmt("  cluster %d: n=%3d  mid-disty=%.3f",
                i, #kid.rows, #kid.rows>0 and mu/#kid.rows or 0)) end
  cs = l.kpp(d, d.rows, 5)
  print("# kpp seed (5 centroids):")
  for i,c in ipairs(cs) do print("  "..i.." "..l.o(c)) end end

-- Prudence checks for new library helpers. Assert-driven; silent on pass.
eg["--check"] = function(_,    xs,s,n,c,a,b,m,z)
  xs = {1,2,2,3,4}
  assert(l.bchop(xs,0)==0 and l.bchop(xs,1)==1
     and l.bchop(xs,2)==3 and l.bchop(xs,5)==5)
  assert(l.bchop(xs,2,true)==1 and l.bchop(xs,3,true)==3)
  assert(l.argmin({5,3,8,1,4}, function(x) return x end) == 1)
  assert(l.argmax({5,3,8,1,4}, function(x) return x end) == 8)
  s = Sym(); for _ = 1,100 do add(s,"a") end
  for _ = 1,10 do add(s,"b") end
  c = {a=0,b=0}; for _ = 1,1000 do c[s:pick()] = c[s:pick()] + 1 end
  assert(c.a > c.b, "sym pick should favor majority")
  n = Num(); for _ = 1,200 do add(n, 10 + rand()*2) end
  a = Num(); for _ = 1,1000 do add(a, n:pick()) end
  assert(abs(a:mid() - n:mid()) < 1, "num pick centers near mu")
  a, b = Num(), Num()
  for _,v in ipairs{1,2,3} do add(a,v) end
  for _,v in ipairs{4,5,6} do add(b,v) end
  m = a:merge(b)
  assert(m.n == 6 and abs(m.mu - 3.5) < 1e-9, "num merge")
  a, b = Sym(), Sym()
  for _,v in ipairs{"a","a","b"} do add(a,v) end
  for _,v in ipairs{"b","c","c"} do add(b,v) end
  z = a:merge(b)
  assert(z.n == 6 and z.has.a == 2 and z.has.b == 2 and z.has.c == 2,
         "sym merge")
  print("--check ok") end

eg["--compare"] = function(f,
    d,runs,out,known,oracle,picks,stat,eps,tied,rows_out,k,xs,s,row,r,_)
  d = Data(f)
  runs = {
    {"sa-r", function(o) row,_ = l.sa(d,o,the.Evals,  0); return row end},
    {"sa+r", function(o) row,_ = l.sa(d,o,the.Evals,100); return row end},
    {"ls-r", function(o) row,_ = l.ls(d,o,the.Evals,  0); return row end},
    {"ls+r", function(o) row,_ = l.ls(d,o,the.Evals,100); return row end},
    {"rand", function(o)
       picks = l.many(d.rows,5)
       l.sort(picks, function(a,b) return d:disty(a) < d:disty(b) end)
       return picks[1] end},
  }
  out = {}; for _,r in ipairs(runs) do out[r[1]] = {} end
  for j = 1, 20 do
    l.shuffle(d.rows)
    known = d:clone(l.slice(d.rows,1,50))
    oracle = function(r) return l.oracleNearest(known,r) end
    for _,r in ipairs(runs) do
      l.push(out[r[1]], d:disty(r[2](oracle))) end end
  stat = function(xs,  s)
    s = Num(); for _,x in ipairs(xs) do add(s,x) end; return s end
  eps = 0.35 * stat(out.rand):spread()
  tied = l.bestRanks(out, eps)
  rows_out = {}
  for k,xs in pairs(out) do
    s = stat(xs)
    local sx = l.sort(l.slice(xs))
    l.push(rows_out, {name=k, best=sx[1], mid=s:mid(), worst=sx[#sx],
                      sd=s:spread(), tied=tied[k] and "*" or "",
                      runs=#xs}) end
  l.sort(rows_out, function(a,b) return a.best < b.best end)
  l.table(rows_out, {"best","mid","worst","sd","runs","tied","name"}) end

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
        l=l,active=active,validate=validate,acquireBayes=acquireBayes}
