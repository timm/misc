-- vim : ft=markdown ts=2 sw=2 et:
--[[

# asdas

asdasdasdasasaasasd asda asd as dasdas
asd asdas

--]]

require "lib"
local c=require "cocomo"

oo(c(i))

local i=0
while i <= #arg do
  i = i+1
  if arg[i] == "--seed" then
    math.randomseed(tonumber(arg[i+1])); i=1+1 end
end
