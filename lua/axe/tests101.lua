local m=require("keys0")
math.randomseed(m.Of.seed)

local o ,oo, go = m.Lib.o, m.Lib.oo, m.Lib.go
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

local function csving()
  for row in m.Lib.csv("data/weather.csv") do 
    print(#row); o(row) end end

local function rowsreading()
  Tbl.read("data/auto93.csv")
end 

--going()
--=csving()
rowsreading()
m.Lib.rogues()
