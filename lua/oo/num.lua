-- vim: nospell:sta:et:sw=2:ts=2:sts=2

require "thing"

Num={up=Thing,show="Num"}

function Num.new()
  return new{ me=Num,mu=0, sd=0, m2=0,
              lo= math.maxinterger,
              hi= math.miningeter}
end

function Num.prep(x) return tostring(x) end

function Num.add1(i,x,       d)
  d    = x - i.mu
  i.mu = i.mu + d/i.n
  i.m2 = i.m2 + d*(x - i.mu)
  i.sd = Num.sd0(i)
  if x > i.max then i.max = x end
  if x < i.min then i.min = x end
end

function Num.sd0(i)
  if     i.n  < 2 then return 0 
  elseif i.m2 < 0 then return 0 
  else            return (i.m2/i.n-1)^0.5 end
end
