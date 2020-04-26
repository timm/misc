--vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 

local x,p,mid,var,divs -- functions

function x(a,z)     
  return a[math.floor(z)] end

function p(a,z)  
  return x(a, z*#i.has ) end

function mid(a,lo,hi) 
  return x(a, lo + .5*(hi-lo) ) end

function var(a,lo,hi) 
  return (x(a,lo+.9*(hi-lo)) - x(lo+.1*(hi-lo)))/2.7 end

function xpect(a,lo,j,hi)
  local n1, n2, n = j-lo+1, hi-j , hi - lo
  return n1/n * var(a,lo,j) + n2/n * var(a,j,hi) end

function divs(a)
  table.sort(a)
  local cuts    = {}
  local step    = math.fllor(#a^THE.some.step)
  local epsilon = var(a,1,#a)
  local function div(lo,hi,     cut)
    local best = var(a,lo,hi)
    for j in lo+step, hi-step do
      local now, after = x(a, j), x(a, j+1)
      if now ~= after then 
        if after - a[1] > epsilon then
	  if a[#a] - now > epsilon then
	    if math.abs( mid(a,lo,j) - mid(a,j,hi) ) > epsilon then
	      local new = xpect(a,lo,j,hi)
	      if new*THE.some.trivial < best then
	        best,cut = new,j end end end end end  end
    return cut  
  end
  local function recurse(lo,hi)
    cut = div(lo,hi)
    if   cut 
    then recurse(lo,cut)
         recurse(cut+1,his) 
    else cuts[ #cuts+1 ] = cut end  end
  recurse(1, #a)
  return cuts
end  

return divs

