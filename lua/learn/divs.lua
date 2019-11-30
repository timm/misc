--vim: ts=2 sw=2 sts=2 expandtab:cindent:
--------- --------- --------- --------- --------- --------- 
 
require "lib"
local THE=require("the").divs

local x,p,mid,var,xpect -- functions

function x(a,z)       return a[math.floor(z)] end
function p(a,z)       return x(a, z*#i.has ) end
function mid(a,lo,hi) return x(a, lo + .5*(hi-lo) ) end

function var(a,lo,hi) 
  return (x(a,lo+.9*(hi-lo)) - x(a,lo+.1*(hi-lo)))/2.7 end

function xpect(a,lo,j,hi)
  local n1, n2, n = j-lo+1, hi-j , hi - lo
  return n1/n * var(a,lo,j) + n2/n * var(a,j,hi) end

return function (a)
  table.sort(a)
  local cuts    = {}
  local step    = math.floor((#a)^THE.step)
  local epsilon = var(a,1,#a)*THE.cohen
  local function div(lo,hi,     cut)
    print("step",step,"lo",lo,"hi",hi)
    local best = var(a,lo,hi)
    for j = lo+step, hi-step do
      local now, after = x(a, j), x(a, j+1)
      if now ~= after then 
        if after - a[1] > epsilon then
	  if a[#a] - now > epsilon then
	    if math.abs( mid(a,lo,j) - mid(a,j,hi) ) > epsilon then
	      local new = xpect(a,lo,j,hi)
	      if new * THE.trivial < best then
	        best,cut = new,j end end end end end  end
    return cut  
  end
  local function recurse(lo,hi)
    cut = div(lo,hi)
    if   cut 
    then recurse(lo,cut)
         recurse(cut+1,his) 
    else cuts[ #cuts+1 ] = a[cut] end  
  end
  recurse(1, #a)
  return cuts
end  
