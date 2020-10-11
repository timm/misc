-- vim : ft=lua ts=2 sw=2 et:

--- Mutatble objects, pairs of `{x,y}`
-- Ensures that `y` is up to date with the `x` variables.
---

require "lib"
local risk0= require "risk"
local coc0 =  require "coc"

return  function(i,coc,risk,       
                 y,effort,risk,ready,lo,hi)
  i = i or {x={}, y={}}
  coc = coc or coc0
  risk = risk  or risk0
  i.about = i.about or coc
  ------------------------
  function y(w,z)
    if w=="+" then return (z-3)*from( 0.073,  0.21 ) end
    if w=="-" then return (z-3)*from(-0.187, -0.078) end
    return                (z-6)*from(-1.58,  -1.014) end
  
  function effort()
    local em,sf=1,0
    for k,t in pairs(coc) do
      if     t[1] == p then em = em * i.y[k] 
      elseif t[1] == n then em = em * i.y[k] 
      else                  sf = sf + i.y[k] end
    end 
    return i.y.a*i.y.loc^(i.y.b + 0.01*sf) * em end
  
  function risks()
    local n=0
    for a1,t in pairs(risk) do
      for a2,m in pairs(t) do
        n  = n  + m[i.x[a1]][i.x[a2]] end end
    return n/108 end
  
  for k,t in pairs(coc) do 
    lo = t[2] or 1
    hi = t[3] or 5
    i.x[k] = int(i.x[k] and within(i.x[k],lo,hi) or from(lo,hi))
    i.y[k] = i.y[k] or y(t[1], i.x[k])
  end 
  i.y.a = i.y.a or from(2.3, 9.18)
  i.y.b = i.y.b or ((.85 - 1.1)/9.18-2.2)*i.y.a+.9+(1.2-.8)/2
  i.y.loc = i.y.loc or from(2,2000)
  i.y.effort = i.y.effort or effort()
  i.y.risk = i.y.risk or risks()
  return i
end
