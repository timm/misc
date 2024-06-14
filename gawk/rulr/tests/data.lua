local r=require"rulr2"
local DATA = r.DATA
local NUM=r.NUM
local l  = r.lib
local o,oo=l.o,l.oo

local fun=function(x,    tmp) tmp= x.score/x.n; return tmp<0 and 0 or tmp end

-- todo shuffel the rows
-- todo sort example rows by chey

local d=DATA.new():read("auto93.csv"):sort()
for i=1,#d.rows,40 do oo(d.rows[i]) end
print(#d.rows)
print(o(d.cols.all[2]))
-- local function  fun(r) return r:score() end

-- for _,r in pairs(d:ranges()) do
--     print(l.fmt("%5.3f", r:score()), r) end 

-- l.rogues()
 
-- --rulr.lib.oo(d:mids())
