local l,m={},{}
function m.settings() return {
  file="../../data/auto93.csv",
  seed=1234567891,
  magic={y="[-+!]$", x="[^-+!]$",min="-$", num="^[A-Z]"}} end

local the=m.settings()

function m.data(strs)
  return {names=  strs,
          xs=     l.kap(strs, function(s) if s:find(the.magic.x) then return s end end)
          ys=     l.kap(strs, function(s) if s:find(the.magic.y) then return s end end)
          heaven= l.kap(strs, function(s) return s:find(the,magic.min) and -1 or 1 end)
          rows= {}, n={}, seen={}, mu={}, m2={}, hi={}, lo={}} end

function m.init(data)
  for c,name in pairs(data.names) do
    data.n[c]=0
    if   name:find(the.magic.num)
    then data.mu[c] = 0
         data.m2[c] = 0
         data.lo[c] =  1E30
         data.hi[c] = -1E30  
    else data.seen[c]={} end end 
  return data end  

function m.nump(data,c) return data.mu[c] end

function m.adds(data,lst) for _,t in pairs(lst) do m.add(data,t) end; return data end
#--------------------------------------------------------------
function l.kap(t,fun,...)
  local u={};for k,v in pairs(t) do u[k]=fun(v,...) end; return u end
