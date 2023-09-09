--  _   _   _ 
-- (/_ (_| _> 
--      _|    

local l  = require"etc"
local eg = require"eg"
local csv,kap,oo,push,sort,test = l.str.csv,l.list.kap,l.str.oo,l.list.push,l.list.sort,l.test
local cos,exp,log,max,min,pi = math.cos,math.exp,math.log,math.max,math.min,math.pi

local DATA,ROW,NUM,SYM = eg.DATA, eg.ROW, eg.NUM, eg.ROW

function test.push(      t)
  t={30,10,20}
  return 40 == push(sort(t), 40) end


