local lib = require"lib"
local l   = {}

function l.per(t,n)  return t[(#t*n)//1] end
function l.median(t) return l.per(t, .5) end
function l.spread(t) return (l.per(t, .9) - l.per(t, .1)) / 2.56 end

function l.stdev(t,    n,d,mu,m2)
  n,mu,m2=0,0,0
  for _,x in pairs(t) do
    n  = n + 1
    d  = x - mu
    mu = mu + d/n
    m2 = m2 + d*(x - mu) end
  return (m2/(n-1))^.5 end

function l.entropy(t, e, N)
  e,N = 0,0
  for _,n in pairs(t) do N = N + n end
  for _,n in pairs(t) do e = e - n/N*math.log(n/N,2) end
  return e end

function l.mode(t, most, mode)
    most = 0
    for k, v in pairs(t) do if v > most then mode, most = k, v end end
    return mode end

return l