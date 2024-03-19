function NUM() return   {n=0, mu=0, m2=0}

function sd(num): return (num.m2/(num.n - 1))^0.5 end

function add(num,x)
  local d = x - num.mu
  num.n   = num.n + 1
  num.mu  = num.mu + d/num.n
  num.m2  = num.m2 + d*(x - num.mu) end
  
function sub(num,x)
  local d = x - num.mu
  num.n   = num.n - 1
  num.mu  = num.mu - d/i.n
  num.m2  = num.m2 - d*(x- num.mu) end
