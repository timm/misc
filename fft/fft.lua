#!/usr/bin/env lua
-- fft.lua, fastmap bi-cluster + jaccard overlap of top-frac leaves
-- (c) 2025, Tim Menzies <timm@ieee.org>, MIT license
local the = {seed=1234567891, trees=20, frac=0.1,
             file="auto93.csv", stop=nil}
local rand, floor, sqrt = math.random, math.floor, math.sqrt
local min, max, HUGE = math.min, math.max, math.huge

local function as(s) return tonumber(s) or s end

local function read(file,    rows, names, cols)
  rows = {}
  for ln in io.lines(file) do
    if ln:sub(1,1) ~= "#" and ln:match"%S" then
      local r = {}
      for x in ln:gmatch"[^,]+" do
        r[#r+1] = as(x:match"^%s*(.-)%s*$") end
      if names then rows[#rows+1] = r else names = r end end end
  cols = {}
  for at,s in ipairs(names) do
    cols[at] = {at=at, txt=s, num = s:match"^%u" ~= nil,
                x = s:sub(-1) ~= "X" and not s:find"[-+!]$",
                y = s:find"[-+!]$" ~= nil,
                heaven = s:sub(-1) == "-" and 0 or 1,
                lo=HUGE, hi=-HUGE}
    if cols[at].num then
      for _,r in ipairs(rows) do local v = r[at]
        if type(v) == "number" then
          cols[at].lo = min(cols[at].lo, v)
          cols[at].hi = max(cols[at].hi, v) end end end end
  return cols, rows end

local function norm(c,v) return (v-c.lo)/(c.hi-c.lo+1E-32) end

local function distx(d, r1, r2,    s, n, v1, v2, row)
  row = d.xs[r1]
  if row and row[r2] then return row[r2] end
  s, n = 0, 0
  for _,c in ipairs(d.cols) do if c.x then
    n = n + 1
    v1, v2 = r1[c.at], r2[c.at]
    if v1=="?" and v2=="?" then s = s + 1
    elseif not c.num then s = s + (v1==v2 and 0 or 1)
    else
      v1 = v1~="?" and norm(c,v1) or (norm(c,v2)>0.5 and 0 or 1)
      v2 = v2~="?" and norm(c,v2) or (v1>0.5 and 0 or 1)
      s = s + (v1-v2)^2 end end end
  s = (s/n)^0.5
  d.xs[r1] = d.xs[r1] or {}; d.xs[r1][r2] = s
  d.xs[r2] = d.xs[r2] or {}; d.xs[r2][r1] = s
  return s end

local function disty(d, r,    s, n, v)
  if d.ys[r] then return d.ys[r] end
  s, n = 0, 0
  for _,c in ipairs(d.cols) do if c.y and c.num then
    v = r[c.at]
    if v ~= "?" then
      n = n + 1; s = s + (norm(c,v) - c.heaven)^2 end end end
  d.ys[r] = n>0 and (s/n)^0.5 or 0
  return d.ys[r] end

local function fastmap(d, stop,    rows, ind, nxt, far, go)
  rows = d.rows
  stop = stop or floor(sqrt(#rows))
  ind, nxt = {}, 0
  far = function(rs, ref)
    table.sort(rs, function(a,b)
      return distx(d,ref,a) < distx(d,ref,b) end)
    return rs[floor(0.99*#rs)] end
  go = function(rs,    a, b, c, m, ls, rss)
    if #rs <= stop then
      nxt = nxt + 1
      for _,r in ipairs(rs) do ind[r] = nxt end
      return end
    a = rs[rand(#rs)]; b = far(rs,a); a = far(rs,b)
    c = distx(d,a,b) + 1E-32
    table.sort(rs, function(p, q)
      local pa, pb = distx(d,p,a), distx(d,p,b)
      local qa, qb = distx(d,q,a), distx(d,q,b)
      return (pa*pa - pb*pb) < (qa*qa - qb*qb) end)
    m, ls, rss = floor(#rs/2), {}, {}
    for i,r in ipairs(rs) do
      if i<=m then ls[#ls+1]=r else rss[#rss+1]=r end end
    go(ls); go(rss) end
  go(rows)
  return ind end

local function jaccards(d, N, frac,    rows, dy, top, jac, parts, out)
  N, frac = N or the.trees, frac or the.frac
  rows = d.rows
  dy = {}
  for _,r in ipairs(rows) do dy[r] = disty(d, r) end
  top = function(ind,    g, arr, k, sub)
    g = {}
    for _,r in ipairs(rows) do
      g[ind[r]] = g[ind[r]] or {set={}, sum=0, n=0}
      local x = g[ind[r]]
      x.set[r] = true; x.sum = x.sum + dy[r]; x.n = x.n + 1 end
    arr = {}
    for _,x in pairs(g) do
      arr[#arr+1] = {mu = x.sum/x.n, set = x.set} end
    table.sort(arr, function(a,b) return a.mu < b.mu end)
    k, sub = max(1, floor(#arr*frac)), {}
    for i=1,k do sub[#sub+1] = arr[i].set end
    return sub end
  jac = function(a,b,    i, u)
    i, u = 0, 0
    for r in pairs(a) do u=u+1; if b[r] then i=i+1 end end
    for r in pairs(b) do if not a[r] then u=u+1 end end
    return i/u end
  parts, out = {}, {}
  for _=1,N do parts[#parts+1] = top(fastmap(d, the.stop)) end
  for i,p in ipairs(parts) do
    for _,s in ipairs(p) do
      local sum, n = 0, 0
      for j,q in ipairs(parts) do if j~=i then
        local best = 0
        for _,t in ipairs(q) do
          local v = jac(s,t); if v>best then best=v end end
        sum, n = sum+best, n+1 end end
      out[#out+1] = sum/n end end
  table.sort(out)
  return out end

-- main ------------------------------------------------------------
for n,v in ipairs(arg) do
  local key = ({s="seed",t="trees",F="frac",f="file"})
              [v:match"^%-(%a)$" or ""]
  if key then the[key] = as(arg[n+1]) end end

math.randomseed(the.seed)
local cols, rows = read(the.file)
if #rows > 2000 then
  for i=#rows,2,-1 do
    local j = rand(i); rows[i], rows[j] = rows[j], rows[i] end
  local keep = {}
  for i=1,2000 do keep[#keep+1] = rows[i] end
  rows = keep end
local d = {cols=cols, rows=rows, ys={}, xs={}}
for _,j in ipairs(jaccards(d)) do print(floor(j*100)) end
