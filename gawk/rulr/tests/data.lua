local r=require"rulr"
local DATA = r.DATA
local NUM=r.NUM
local l  = r.lib

local fun=function(x,    tmp) tmp= x.score/x.n; return tmp<0 and 0 or tmp end

-- todo shuffel the rows
-- todo sort example rows by chey

local d=DATA.new(l.csv("auto93.csv"))
for _,col in pairs(d.cols.x) do
    print("\n" .. col.name)
   for _,r in pairs(l.sort(col.ranges, l.on(fun))) do 
       print(l.fmt("%5.3f",fun(r)), l.o(r)) end end

l.rogues()
 
--rulr.lib.oo(d:mids())