local l=require("rulr").lib


local c,n = 0,0
for t in l.csv("auto93.csv") do
  n = n+1
  if n==1 then print("        ",l.o(t)) end
  c = c + #t
  if n % 30==0 then print("        ", l.o(t)) end end
