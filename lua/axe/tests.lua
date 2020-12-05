local m=require("axe")
math.randomseed(m.Of.seed)

local o ,oo, any = m.Lib.o, m.Lib.oo, m.Lib.any
local Num, Sym,Tbl  = m.Num, m.Sym, m.Tbl

local function going(      x,y) 
  x = Sym.new(1,"names")
  y = Num.new(1,"door")
  x:add("love")
  x:add("hate")
  x:add("hate")
  y:add(20)
  y:add(30)
  oo(x)
  oo(y) end

local function csving(   n)
  for row in m.Lib.csv("data/weather.csv") do 
    n = n or #row
    assert(#row == n) end end

local function rowsreading()
  local t = Tbl.read("data/auto93.csv")
  assert(#t.rows==398) end 

local function rowsdist()
  local t = Tbl.read("data/weather.csv")
  for i=1,10 do
    local r1,r2 = any(t.rows), any(t.rows)
    assert(0== r1:dist(r1,t)) 
    print(r1:dist(r2,t)) end end

--going()
--csving()
--rowsreading()
rowsdist()
m.Lib.rogues()

