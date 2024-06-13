local r=require"rulr"
local DATA = r.DATA
local l  = r.lib

local fun=function(x) return l.rnd(x.score/x.n) end

-- todo shuffel the rows

d=DATA.new(l.csv("auto93.csv"))
for _,col in pairs(d.cols.x) do
    print""
   for _,r in pairs(l.sort(col.ranges, l.on(fun))) do 
       print(l.o(r), fun(r)) end end
 
--rulr.lib.oo(d:mids())