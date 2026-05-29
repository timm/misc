#!/usr/bin/env lua
local the,help = {}, [[
fft.lua, multi objective tree building
(c) 2025, Tim Menzies <timm@ieee.org>, MIT license

Options:
 -s --random seed   seed=1234567891
 -d --depth of tree depth=4
 -b --bins  bins    bins=5
 -n --N     samples N=64
 -f data file       file=auto93.csv
]]

local min,max,HI = math.min, math.max, math.huge
local floor,sqrt,abs,random = math.floor, math.sqrt, math.abs, math.random
local push,trim,as,dataClone,dataHeader,dataRead
local Num,Sym,Data,isSym,isData,add,adds
local norm,disty,distyMean,binOf,worstBins,peel,sample,main
local boxOf,inBox,models,binLabel,showRows,yMeans,explainRound

push = function(t,x) t[1+#t]=x; return x end
trim = function(s) return s:match"^%s*(.-)%s*$" end
as = function (s)
      return s=="True" or (s~="False" and (tonumber(s) or s)) end

for k,v in help:gmatch"(%w+)=(%S+)" do the[k] = as(trim(v)) end

-----------------------------------------------------------------
function Data()      return {rows={},cols={}} end
function Sym(at,txt) return {at=at or 0,txt=txt or "",has={}} end
function Num(at,txt) return {at=at or 0,txt=txt or "",
	                     goal=(txt or ""):find"-$" and -1 or 1,
	                     lo=HI, hi=-HI, mu=0, n=0} end

function isSym(c)  return c.has  ~= nil end
function isData(c) return c.rows ~= nil end

function add(i,v,    t,d)
  if v=="?" then return v end
  if isSym(i) then i.has[v] = 1 + (i.has[v] or 0)
  elseif isData(i) then
    if next(i.cols) then
      t={}; for _,c in ipairs(i.cols.all) do 
              push(t, add(c,v[c.at])) end
      push(i.rows,t)
    else i.cols = dataHeader(v) end
  else
    i.n = i.n + 1
    d = v - i.mu
    i.mu = i.mu + d/i.n 
    i.lo = min(i.lo, v)
    i.hi = max(i.hi, v) end
  return v end

function adds(src,i)
  i = i or Num()
  if type(src)=="function" then for x in src do add(i,x) end
  else for _,x in ipairs(src or {}) do add(i,x) end end
  return i end

-----------------------------------------------------------------
function dataClone(data, rows)
  return adds(rows, adds({data.cols.names}, Data())) end

function dataHeader(names,    i,col,last)
  i = {all={}, x={}, y={}, klass=nil, names=names}
  for n,s in ipairs(names) do
    col = push(i.all, s:match"^%u" and Num(n,s) or Sym(n,s))
    if not s:find"X$" then
      if s:find"!$" then i.klass = col end
      push(s:find"[-+!]$" and i.y or i.x, col) end end
  i.klass = i.klass or i.y[1]
  return i end

function dataRead(file,    d,t)
  d = Data()
  for line in io.lines(file) do
    if line:sub(1,1) ~= "#" and line:match"%S" then
      t={}; for x in line:gmatch"[^,]+" do push(t, as(x)) end
      add(d, t) end end
  return d end

-- ## disty: distance to heaven, min-max normed over y-cols (p=2) -----
-- heaven = 1 for maximize (+), 0 for minimize (-). root supplies lo/hi.
function norm(col,v)
  if v=="?" then return v end
  return (v - col.lo)/(col.hi - col.lo + 1E-32) end

function disty(root,row,    d,n,heaven)
  d,n = 0,0
  for _,c in ipairs(root.cols.y) do
    heaven = c.goal==1 and 1 or 0
    n = n+1
    d = d + abs(norm(c,row[c.at]) - heaven)^2 end
  return (d/n)^0.5 end

function distyMean(root,rows,    s)
  s = Num(); for _,r in ipairs(rows) do add(s, disty(root,r)) end
  return s.mu end

-- ## bins: Sym -> value. Num -> 0..bins-1 (min-max). live col supplies lo/hi.
function binOf(col,v,    z)
  if v=="?" then return "?" end
  if isSym(col) then return v end
  if col.hi <= col.lo then return 0 end
  z = (v - col.lo)/(col.hi - col.lo)
  return min(the.bins-1, floor(the.bins*z)) end

-- ## worst ranges: top-k single-range bins by mean disty (highest = worst).
-- bin membership uses live `dd` x-stats; disty uses fixed `root` y-stats.
function worstBins(dd,root,k,    acc,all,y,bad,bb)
  acc = {}
  for _,row in ipairs(dd.rows) do
    y = disty(root,row)
    for _,c in ipairs(dd.cols.x) do
      bb = binOf(c, row[c.at])
      if bb ~= "?" then
        acc[c.at] = acc[c.at] or {}
        acc[c.at][bb] = acc[c.at][bb] or Num(c.at)
        add(acc[c.at][bb], y) end end end
  all = {}
  for _,c in ipairs(dd.cols.x) do
    for bk,num in pairs(acc[c.at] or {}) do
      push(all, {col=c, b=bk, mu=num.mu}) end end
  table.sort(all, function(p,q) return p.mu > q.mu end)
  bad = {}
  for j=1, min(k,#all) do push(bad, all[j]) end
  return bad end

-- ## box: per-x-attr range of survivors, but ONLY carved attrs (tighter
-- than root). Sym -> set of survivor values; Num -> [lo,hi]. This is the model.
function boxOf(root,rows,    box,lo,hi,set,m,rm,v)
  box = {}
  for _,c in ipairs(root.cols.x) do
    if isSym(c) then
      set,m = {},0
      for _,r in ipairs(rows) do v=r[c.at]
        if v~="?" and not set[v] then set[v]=true; m=m+1 end end
      rm=0; for _ in pairs(c.has) do rm=rm+1 end
      if m>0 and m<rm then box[c.at]={set=set} end          -- carved
    else
      lo,hi=HI,-HI
      for _,r in ipairs(rows) do v=r[c.at]
        if v~="?" then lo=min(lo,v); hi=max(hi,v) end end
      if lo<=hi and (lo>c.lo or hi<c.hi) then box[c.at]={lo=lo,hi=hi} end
    end end
  return box end

function inBox(box,row,    v)
  for at,spec in pairs(box) do
    v = row[at]
    if v=="?" then return false end
    if spec.set then if not spec.set[v] then return false end
    elseif v<spec.lo or v>spec.hi then return false end end
  return true end

-- label a bad bin as "ColName op value" for tracing
function binLabel(bd,    c,w,lo)
  c = bd.col
  if isSym(c) then return c.txt.."="..tostring(bd.b) end
  w  = (c.hi - c.lo)/the.bins
  lo = c.lo + bd.b*w
  return string.format("%s∈[%.2g,%.2g]", c.txt, lo, lo+w) end

-- ## peel: vote rows by #bad-ranges hit, keep bottom `frac`, halve to sqrt(n).
-- returns survivors + free best/rest labels (box membership over root) + box.
function peel(root,k,frac,trace,    live,dd,bad,scored,v,keep,m,box,best,rest,rnd,labs)
  live, rnd = root.rows, 0
  if trace then
    print(string.format("  %-4s %-5s %-7s  %s","rnd","n","disty","ranges removed"))
    print(string.format("  %-4s %-5d %-7.3f","-",#live,distyMean(root,live))) end
  while #live > sqrt(#root.rows) do
    dd  = dataClone(root, live)              -- re-discretize on shrinking set
    bad = worstBins(dd, root, k)
    scored = {}
    for _,row in ipairs(dd.rows) do
      v = 0
      for _,bd in ipairs(bad) do
        if binOf(bd.col, row[bd.col.at]) == bd.b then v = v+1 end end
      push(scored, {row=row, v=v}) end
    table.sort(scored, function(a,b) return a.v < b.v end)
    keep, m = {}, floor(#scored*frac)
    if m < 1 then break end
    for j=1, m do push(keep, scored[j].row) end
    if #keep >= #live then break end          -- no progress
    live, rnd = keep, rnd+1
    if trace then
      labs = {}; for _,bd in ipairs(bad) do push(labs, binLabel(bd)) end
      print(string.format("  %-4d %-5d %-7.3f  %s",
            rnd, #live, distyMean(root,live), table.concat(labs,"  "))) end end
  box  = boxOf(root, live)
  best, rest = {}, {}
  for _,r in ipairs(root.rows) do
    push(inBox(box,r) and best or rest, r) end
  return {rows=live, vol=#live/#root.rows, mu=distyMean(root,live),
          box=box, best=best, rest=rest} end

-- ## models: sample random sub-boxes of a peel's box, score pd/pf vs its
-- free best/rest. Each sub-box = random attr subset, each shrunk at random.
function models(res,N,    out,ats,sub,spec,a,b,s2,any,tp,fp,p)
  out, ats = {}, {}
  for at in pairs(res.box) do push(ats, at) end
  if #ats==0 then return out end
  p = min(1, 2/#ats)                       -- expect ~2 attrs per model
  for _=1,N do
    sub = {}
    repeat
      for _,at in ipairs(ats) do
        if random()<p then
          spec = res.box[at]
          if spec.set then
            s2,any = {},false
            for vv in pairs(spec.set) do
              if random()<0.6 then s2[vv]=true; any=true end end
            if any then sub[at]={set=s2} end
          elseif random()<0.5 then         -- full survivor range
            sub[at] = {lo=spec.lo, hi=spec.hi}
          else                             -- random tighter sub-interval
            a = spec.lo + random()*(spec.hi-spec.lo)
            b = spec.lo + random()*(spec.hi-spec.lo)
            sub[at] = {lo=min(a,b), hi=max(a,b)} end end end
    until next(sub)
    tp,fp = 0,0
    for _,r in ipairs(res.best) do if inBox(sub,r) then tp=tp+1 end end
    for _,r in ipairs(res.rest) do if inBox(sub,r) then fp=fp+1 end end
    push(out, {box=sub,
               pd = #res.best>0 and tp/#res.best or 0,
               pf = #res.rest>0 and fp/#res.rest or 0}) end
  return out end

-- ## sample: the model generator. Randomize bins/k/frac -> cloud of peels.
function sample(root,N,    cloud,k,frac,r)
  cloud = {}
  for _=1,N do
    the.bins = 2 + floor(random()*7)          -- 2..8
    k        = 1 + floor(random()*3)          -- 1..3
    frac     = random()<0.5 and 0.5 or 0.66
    r = peel(root, k, frac)
    r.bins, r.k, r.frac = the.bins, k, frac
    push(cloud, r) end
  return cloud end

-- ## explainRound: show round-1 decision. Every chunk (col x bin) with its
-- average badness (disty), grouped by col, worst-first; mark the k killed.
function explainRound(root,k,    dd,acc,bb,y,all,kill,chunks,key)
  dd, acc = dataClone(root, root.rows), {}
  for _,row in ipairs(dd.rows) do
    y = disty(root,row)
    for _,c in ipairs(dd.cols.x) do
      bb = binOf(c, row[c.at])
      if bb~="?" then
        acc[c.at] = acc[c.at] or {}
        acc[c.at][bb] = acc[c.at][bb] or Num(c.at)
        add(acc[c.at][bb], y) end end end
  all = {}
  for _,c in ipairs(dd.cols.x) do
    for b,num in pairs(acc[c.at] or {}) do
      push(all, {col=c, b=b, mu=num.mu}) end end
  table.sort(all, function(p,q) return p.mu>q.mu end)
  kill = {}
  for i=1, min(k,#all) do kill[all[i].col.at..":"..tostring(all[i].b)]=true end
  print(string.format("  %-42s %-4s %-8s %s","chunk (column + range)","n","badness","kill"))
  for _,c in ipairs(dd.cols.x) do
    chunks = {}
    for b,num in pairs(acc[c.at] or {}) do push(chunks,{b=b,mu=num.mu,n=num.n}) end
    table.sort(chunks, function(p,q) return p.mu>q.mu end)
    for _,ch in ipairs(chunks) do
      key = c.at..":"..tostring(ch.b)
      print(string.format("  %-42s %-4d %-8.3f %s",
        binLabel({col=c,b=ch.b}), ch.n, ch.mu, kill[key] and "★ KILL" or "")) end end end

-- print rows as a table under the column header.
function showRows(d,rows,    cell)
  print("  "..table.concat(d.cols.names, "  "))
  for _,r in ipairs(rows) do
    cell = {}
    for _,c in ipairs(d.cols.all) do
      push(cell, string.format("%"..max(#c.txt,5).."s", tostring(r[c.at]))) end
    print("  "..table.concat(cell, "  ")) end end

-- mean of each y-col over rows, vs whole data, with goal arrow.
function yMeans(d,rows,    s,all)
  for _,c in ipairs(d.cols.y) do
    s,all = Num(),Num()
    for _,r in ipairs(rows)   do add(s,  r[c.at]) end
    for _,r in ipairs(d.rows) do add(all,r[c.at]) end
    print(string.format("  %-12s goal=%s  all=%.2f  survivors=%.2f",
          c.txt, c.goal==1 and "max" or "min", all.mu, s.mu)) end end

-- ## main ------------------------------------------------------------
function main(    d,t0,t1,t2,t3,one,cloud,top,ms,nat)
  for n,v in ipairs(arg) do
    local key = ({s="seed",d="depth",b="bins",N="N",f="file"})[v:match"^%-(%a)$" or ""]
    if key then the[key] = as(arg[n+1]) end end
  math.randomseed(the.seed)
  t0 = os.clock(); d = dataRead(the.file); t1 = os.clock()

  the.bins = 3
  print(string.format("\n=== ROUND 1 decision: every chunk's badness (bins=3, kill worst 3) ==="))
  explainRound(d, 3)

  print(string.format("\n=== ONE peel (bins=3 k=3 frac=0.66) on %s ===", the.file))
  one = peel(d, 3, 0.66, true)
  nat = 0; for _ in pairs(one.box) do nat = nat+1 end
  t2 = os.clock()

  print(string.format("\n--- %d surviving rows (of %d) ---", #one.rows, #d.rows))
  showRows(d, one.rows)
  print(string.format("\n--- y-means: survivors vs all (carved attrs=%d) ---", nat))
  yMeans(d, one.rows)

  the.bins = 4
  cloud = sample(d, the.N)
  table.sort(cloud, function(a,b) return a.mu < b.mu end)
  top = cloud[1]
  ms  = models(top, the.N)
  table.sort(ms, function(a,b) return (a.pd-a.pf) > (b.pd-b.pf) end)
  t3 = os.clock()
  print(string.format("\n--- cloud of %d peels: disty min=%.3f median=%.3f max=%.3f ---",
        #cloud, cloud[1].mu, cloud[floor(#cloud/2)].mu, cloud[#cloud].mu))
  print(string.format("--- best peel -> %d random sub-box models (best=%d rest=%d) ---",
        #ms, #top.best, #top.rest))
  print(string.format("  %-6s %-6s %-6s","pd","pf","pd-pf"))
  for i=1, min(5,#ms) do local m=ms[i]
    print(string.format("  %-6.2f %-6.2f %-6.2f", m.pd, m.pf, m.pd-m.pf)) end

  print(string.format("\n--- runtimes ---"))
  print(string.format("  read   %.3fs", t1-t0))
  print(string.format("  1 peel %.4fs", t2-t1))
  print(string.format("  %d peels + %d models  %.3fs", #cloud, #ms, t3-t2))
  print(string.format("  TOTAL  %.3fs", t3-t0)) end

main()
