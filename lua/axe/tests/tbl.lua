package.path='../src/?.lua;'.. package.path 
local l=require "lib"
local t=require "tbl"

math.randomseed(1)

local o ,oo, any = l.o, l.oo, l.any
local Num, Sym,Tbl  = t.Num, t.Sym, t.Tbl

local function going(      x,y,z)
  x = Sym.new()
  y = Num.new()
  x:add("love")
  x:add("hate")
  x:add("hate")
  y:add(20)
  y:add(30)
  assert(2==x.seen.hate,"counting symbols")
  assert(25==y.mu,"mean") 
  z=Num.new()
  for _,x in pairs{9,2,5,4,12,7,8,11,9,
                   3,7,4,12,5,4,10,9,6,9,4} do z:add(x) end
  assert(7 == z.mu,"mu")
  assert(3.06 <= z.sd and z.sd <= 3.061,"sd")
end

local function csving(   m,n)
  m=-1
  for row in l.csv("../data/weather.csv") do
    n = n or #row
    m=m+1
    if m>0 then assert("number" == type(row[2]),"is number") end
    assert(#row == n,"rows right") end 
  end

local function rowsreading()
  local tbl = Tbl.read("../data/auto93.csv")
  assert("Num"==tbl.cols[2].ako,"is Num")
  assert(#tbl.rows==398,"auto rows") end

local function rowsdist()
  local tbl = Tbl.read("../data/weather.csv")
    local r1,r2 = tbl.rows[3], tbl.rows[4]
    o(r1.cells); o(r2.cells)
    print( r1:dist(r2,tbl.xs) ) end

local function rowsdists()
  local tbl = Tbl.read("../data/auto93.csv")
  local all={}
  for i=1,10 do
    local r1,r2 = any(tbl.rows), any(tbl.rows)
    local one ={r1=r1.cells, r2=r2.cells}
    one.d = r1:dist(r2,tbl.xs) 
    all[#all+1] = one
    assert(0== r1:dist(r1,tbl.xs),"sane distance "..i) end
  table.sort(all, function(x,y) return x.d < y.d end)
  for _,one in pairs(tbl.xs) do
     print(one.pos) end
  for _,one in pairs(all) do
     print(""); print(one.d); o(one.r1); o(one.r2) end end

--going()
--csving()
--rowsreading()
--rowsdist()
rowsdists()
l.rogues()

