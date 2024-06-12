local l=dofile("../src/rulr.lua").lib


local c,n = 0,0
for t in l.csv("auto93.csv") do
  n = n+1
  c = c + #t
  if n % 30==0 then print(n, l.o(t)) end end

assert(c==3192,"bad read")
