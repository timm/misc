--  __                   __                    ___                        
-- /\ \__               /\ \__                /\_ \                       
-- \ \ ,_\    __    ____\ \ ,_\   ____        \//\ \    __  __     __     
--  \ \ \/  /'__`\ /',__\\ \ \/  /',__\         \ \ \  /\ \/\ \  /'__`\   
--   \ \ \_/\  __//\__, `\\ \ \_/\__, `\      __ \_\ \_\ \ \_\ \/\ \L\.\_ 
--    \ \__\ \____\/\____/ \ \__\/\____/     /\_\/\____\\ \____/\ \__/.\_\
--     \/__/\/____/\/___/   \/__/\/___/      \/_/\/____/ \/___/  \/__/\/_/
                                                                       
local eg = require"eg"
local l  = require"lib"
local csv,oo,push,sort,test = l.str.csv,l.str.oo,l.list.push,l.list.sort,l.test
local cos,exp,log,max,min,pi = math.cos,math.exp,math.log,math.max,math.min,math.pi

local DATA,ROW,NUM,SYM = eg.DATA, eg.ROW, eg.NUM, eg.ROW

function test.the_show_settings() 
  oo(eg.the) end

function test.round_nums()
  for _,i in pairs{-3.1,-3.7, 10,"asda",3.1,3.7} do
    print(i, l.maths.rnd(i)) end end

function test.push_basic_test(      t)
  t={30,10,20}
  return 40 == push(sort(t), 40) end

function test.map_demo(t)
  oo(l.list.map({1,2,3}, function(x) if x > 1 then return x*10 end end)) end

function test.copy_copy_nested_strucures(t,    u)
  t={1,2,{4,5,{6,7}}}
  u=l.list.copy(t)
  t[3][3][2]=7000
  return t[3][3][2] ~= u[3][3][2] end

test.Run(eg.the)
