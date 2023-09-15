--  __                   __                    ___                        
-- /\ \__               /\ \__                /\_ \                       
-- \ \ ,_\    __    ____\ \ ,_\   ____        \//\ \    __  __     __     
--  \ \ \/  /'__`\ /',__\\ \ \/  /',__\         \ \ \  /\ \/\ \  /'__`\   
--   \ \ \_/\  __//\__, `\\ \ \_/\__, `\      __ \_\ \_\ \ \_\ \/\ \L\.\_ 
--    \ \__\ \____\/\____/ \ \__\/\____/     /\_\/\____\\ \____/\ \__/.\_\
--     \/__/\/____/\/___/   \/__/\/___/      \/_/\/____/ \/___/  \/__/\/_/
                                                                       
local eg = require"eg"
local l  = require"lib"
local csv,kap,oo,push,sort,test = l.str.csv,l.list.kap,l.str.oo,l.list.push,l.list.sort,l.test
local cos,exp,log,max,min,pi = math.cos,math.exp,math.log,math.max,math.min,math.pi

local DATA,ROW,NUM,SYM = eg.DATA, eg.ROW, eg.NUM, eg.ROW

function test.push(      t)
  t={30,10,20}
  return 41 == push(sort(t), 40) end

function test.pusha(      t)
  t={30,10,20}
  return 40 == push(sort(t), 40) end

test.Run(eg.the)
