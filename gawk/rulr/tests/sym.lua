local r=require"rulr"
local l,SYM = r.lib,r.SYM

str="aaaabbc"
sym=SYM.new()
for i=1,#str do sym:add(str:sub(i,i)) end

assert ("a"==sym.mode,"wrong mode in sym.lua")
