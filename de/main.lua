-- vim: ft=lua ts=2 sw=2 et:
-- (c) Tim Menzies, timm@ieee.org 2020,BSD 2-clause license

require "lib"
local c=require "cocomo"

oo(c(i))

local i=0
while i <= #arg do
  i = i+1
  if arg[i] == "--seed" then
    math.randomseed(tonumber(arg[i+1])); i=1+1 end
end
