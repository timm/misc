-- Lib.lua : misc lua tools
-- (c) 2024 Tim Menzies, timm@ieee.org, BSD-2 license

local maths={}

-- Lin, J.T. (1989). Approximating the Normal Tail Probability and its 
-- Inverse for use on a Pocket Calculator. Applied Statistics, 38, 69-70.
function maths.auc(x,mu,sigma)
  cdf = function(z) return 1 - 0.5*2.718^(-0.717*z - 0.416*z*z) end
  z = (x - self.mu) / self.sd
  return z >= 0 and cdf(z) or 1 - cdf(-z) end

-- distance =  maximum of the distances in each coordinate.
function maths.chebyshev(row,cols,      c,tmp)
  c = 0
  for _,col in pairs(cols) do
    tmp = col:norm(row[col.pos]) -- normalize  0..1 
    c = math.max(c, math.abs(col.best - tmp)) end
  return 1 - c end -- so LARGER values are better

-- distance = sum of column distance^p, all ^(1/p) at end.
function maths.minkowski(row,cols,  p,     d,n)
  d,n = 0,0
  for _,col in pairs(cols) do
    n=n+1
    d = d + col:dist(row1[col.pos], row2[col.pos])^p end
  return (d/n)^(1/p) end

function maths.norms(mu,sd)
   return mu + sd* math.log(1/math.random())^.5*math.cos(math.pi*math.random()) end

function maths.welford(n,mu,m2,     d)
  d  = x  - mu
  mu = mu + d/n
  m2 = m2 + d(*x- mu)
  sd = (m2/(n-1+1E-30))^0.5
  return mu,m2,sd end

return maths
