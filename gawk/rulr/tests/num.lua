local r=require"rulr"
local l,NUM= r.lib,r.NUM

math.randomseed(1234567891)
num=NUM.new()
for _ =1,1000 do num:add(math.random()^0.5) end

for x=0,1,0.02 do 
  local a=l.rnd(num:area(x),2)
  print(x, a, l.rnd(num:range(x)), ("*"):rep(a * 50 // 1)) end

assert(0.671 == l.rnd(num.mu,3),"bad mu")
assert(0.237 == l.rnd(num.sd,3),"bad sd")