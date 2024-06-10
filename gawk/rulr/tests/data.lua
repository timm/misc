local rulr=dofile("../rulr.lua")
local DATA = rulr.DATA
local csv  = rulr.lib.csv

d=DATA.new(csv("auto93.csv"))
rulr.lib.oo(d:mids())

