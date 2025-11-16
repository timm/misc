-- (c) 2025, Tim Menzies, MIT license
-- timm-style rewrite of the gawk splitter/ranker

local b4={}; for i,_ in pairs(_ENV) do b4[i]=i end

BINS=7
Names,Num,Sym,X,Y={}, {}, {}, {}, {}
N,Mu,M2,Sd={}, {}, {}, {}
Freq,Row={}, {}
NR=0

-- ---------------------------------------------
function push(t,x) t[#t+1]=x end

function welford(c,v)
  v=tonumber(v)
  N[c]=(N[c] or 0)+1
  local d=v-(Mu[c] or 0)
  Mu[c]=(Mu[c] or 0)+d/N[c]
  M2[c]=(M2[c] or 0)+d*(v-Mu[c])
  Sd[c]=N[c]<2 and 0 or math.sqrt(M2[c]/(N[c]-1))
  return v end

function norm(c,v)
  return 1/(1+math.exp(-1.7*(v-Mu[c])/(Sd[c]+1e-32))) end

function bin(c,v)
  if v=="?" then return v end
  if Sym[c] then return v end
  return math.floor(BINS*norm(c,v)) end

-- ---------------------------------------------
function disty()
  local d,n=0,0
  for c,_ in pairs(Y) do
    n=n+1
    d=d+(norm(c,Row[c])-(Y[c] and 1 or 0))^2 end
  return math.sqrt(d/n) end

function count(d)
  local k=9999
  welford(k,d)
  local best= norm(k,d) <= math.sqrt(NR-1)/(NR-1)
  for c,_ in pairs(X) do
    local v=bin(c,Row[c])
    Freq[c]=Freq[c] or {}
    Freq[c][v]=Freq[c][v] or {[0]=0,[1]=0}
    local t=Freq[c][v]
    t[best and 1 or 0]=t[best and 1 or 0]+1 end end

-- ---------------------------------------------
function report()
  local B=math.sqrt(NR-1)
  local R=(NR-1)-math.sqrt(B)
  local a,i={},0
  for c,vs in pairs(Freq) do
    for v,t in pairs(vs) do
      local b=(t[1] or 0)/B
      local r=(t[0] or 0)/R
      i=i+1
      a[i]={["="]=b*b/(b+r+1e-32),col=c,val=v} end end
  table.sort(a,function(x,y) return x["="]>y["="] end)
  for i,z in ipairs(a) do print(i,z["="],z.col,z.val) end end

-- ---------------------------------------------
function rogues()
  for i,_ in pairs
  lo
-- ---------------------------------------------
-- main
for line in io.lines() do
  NR=NR+1
  local t={}
  for s in line:gmatch("[^,]+") do push(t, s:gsub("[ \t]+","")) end
  if NR==1 then
    for c,v in ipairs(t) do
      Names[c]=v
      if v:match("^[A-Z]") then Num[c]=true else Sym[c]=true end
      if v:match("%+$") then Y[c]=1
      elseif v:match("%-$") then Y[c]=0
      else X[c]=true end end
  else
    for c,_ in pairs(Num) do
      if t[c]~="?" then t[c]=welford(c,t[c]) end end
    Row=t
    count(disty()) end end

report(); rogues()

