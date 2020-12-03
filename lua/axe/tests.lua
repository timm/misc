local m=require("axe")

local o ,oo, go = m.lib.o, m.lib.oo, m.lib.go
local num, sym  = m.num, m.sym

local function going(      x,y) 
  x = sym.new("addss",1)
  y = num.new("adds",1)
  go(x,"add","asdas")
  go(y,"add",23)
  oo(x)
  oo(y) end

local function fastgoing(    y,n,t1,t2,t3) 
  y = num.new("adds",1)
  n  = 10^6
  t1 = os.time()
  for i=1,n do go(y,"add",23) end
  t2 = os.time()
  for i=1,n do m.num.add(y,23) end
  t3 = os.time()
  t3 = os.difftime(t3,t2)/n
  t2 = os.difftime(t2,t1)/n
  print(t2,t3,t2/t3) end

local function csving()
  for row in m.lib.csv("data/weather.csv") do 
    print(o(row)) end end

local function rowsreading( i)
  i = rows.new()
  rows.read(i,"data/weather.csv")
  oo(i) end 

math.randomseed(m.of.seed)
going()
-- fastgoing()
csving()
rowsreading()
m.lib.rogues()
