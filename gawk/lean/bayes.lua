--<!-- vim: set syntax=lua ts=2 sw=2 et : -->
local l={}


-- How much does a column like one value `any`?
function l.like(col1,any,prior,    nom,denom)
  if col1.isSym then
    return ((col1.has[any] or 0) + the.m*prior)/(col1.n+the.m) 
  else
    if any > col1.mu + 4*col1.sd then return 0 end
    if any < col1.mu - 4*col1.sd then return 0 end
    nom   = math.exp(-.5*((any - col1.mu)/col1.sd)^2)
    denom = (col1.sd*((2*math.pi)^0.5))
    return nom/(denom  + 1E-30) end end

-- How much does a `data` like the row `rowt`?
function l.likes(rowt,data,n,h,       prior,out,col1,inc)
  prior = (#data.rows + the.k) / (n + the.k * h)
  out   = math.log(prior)
  for at,v in pairs(rowt) do
    col1 = data.cols.x[at]
    if col1 and v ~= "?" then
      inc = l.like(col1,v,prior)
      out = out + math.log(inc) end end
  return out end

-- which `data` in `datas` does the row `rowt` like most?
function l.likesMost(rowt,datas,n,h,     most,tmp,out)
  most = -1E30 
  for k,data in pairs(datas) do
    tmp = l.likes(rowt,data,n,h)
    if tmp > most then out,most = k,tmp end end
  return out,most end
  
return l