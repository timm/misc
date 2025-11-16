-- (c) 2025 Tim Menzies, MIT
local b4={}; for k,_ in pairs(_ENV) do b4[k]=k end
local the={bins=7}

-------------------------------------------------------------------------------
function Num(at,txt)
  return {at=at, txt=txt or"", isNum=true, n=0, mu=0, m2=0, sd=0, isNum=true,
          best=(txt or""):find"-$"and 0 or 1} end

function Data() return {rows={}, freq={}, cols=nil, dist=Num()} end

function Sym(at,txt) return {at=at, txt=txt or "", seen={}} end

function Cols(h,    all,x,y)
  all,x,y = {}, {}, {}
  for c,v in ipairs(h) do
    all[c] = (v:match"^[A-Z]"and Num or Sym)(c,v)
    (v:find"[+-]$" and y or x)[c] = all[c] end
  return {all=all, x=x, y=y} end

-------------------------------------------------------------------------------
function welford(col,v,    d)
  col.n  = col.n + 1
  d      = v - col.mu
  col.mu = col.mu + d/col.n
  col.m2 = col.m2 + d*(v-col.mu)
  col.sd = col.n<2 and 0 or math.sqrt(col.m2/(col.n-1))
  return v end

function norm(col,v) return 1/(1+math.exp(-1.7*(v-col.mu)/(col.sd+1e-32))) end

function bin(col,v)
  return col.isNum and v~="?" and math.floor(the.bins*norm(col,v)) or v end

function disty(d,row,    dd,n)
  dd,n=0,0
  for _,col in pairs(d.cols.y) do
    n  = n + 1
    dd = dd + (norm(col,row[col.at]) - col.best)^2 end
  return math.sqrt(dd/n) end

function count(d,row,x,    best,n)
  welford(d.dist,x)
  n = #d.rows
  best = norm(d.dist,x) <= math.sqrt(n-1)/(n-1) and 1 or 0
  for c,col in pairs(d.cols.x) do
    inc2(d.freq, c, bin(col,row[c]), best) end end

function report(d,    lst,B,R,n,z)
  lst = {}
  n = #d.rows
  B = math.sqrt(n)
  R = n-math.sqrt(B)
  for c,vs in pairs(d.freq) do
    for v,t in pairs(vs) do
      local b=(t[1] or 0)/B
      local r=(t[0] or 0)/R
      push(lst, {score=b*b/(b+r+1e-32), col=c, val=v}) end end
  table.sort(lst, function(a,b) return a.score > b.score end)
  for i,z in ipairs(lst) do
    print(i, z.score, z.col, z.val) end end

function rogues()
  for k,_ in pairs(_ENV)do if not b4[k]then io.stderr:write(" ?",k) end end end

-------------------------------------------------------------------------------
function push(t,x) t[#t+1]=x end

function inc2(f,c,v,b,    t)
  f[c]=f[c]or{}
  t=f[c][v]or{[0]=0,[1]=0}
  f[c][v]=t
  t[b]=t[b]+1 end

-------------------------------------------------------------------------------
local data = Data()
for line in io.lines() do
  local row={}
  for s in line:gmatch"[^,]+"do
    push(row,s:gsub("%s+","")) end
  if not data.cols
  then data.cols = Cols(row)
  else
    for c,col in pairs(data.cols.all) do
      if col.isNum and row[c]~="?" then
        row[c] = welford(col, tonumber(row[c])) end end
    push(data.rows, row)
    count(data, row, disty(data,row)) end end

report(d)
rogues()
