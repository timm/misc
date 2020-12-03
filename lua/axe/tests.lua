local m=require("axe")

local x = m.sym.new("addss",1)
local y = m.num.new("adds",1)
m.lib.go(x,"add","asdas")
m.lib.go(y,"add",23)
m.lib.oo(x)
m.lib.oo(y)

m.lib.rogues()

local n=10^7
local t1=  os.time()
for i=1,n do m.lib.go(y,"add",23) end
local t2=  os.time()
for i=1,n do m.num.add(y,23) end
local t3=  os.time()
t3=os.difftime(t3,t2)/n
t2=os.difftime(t2,t1)/n
print(t3)
print(t2,t3,t2/t3)

