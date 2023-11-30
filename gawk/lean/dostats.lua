#!/usr/bin/env lua
local lib   = require"lib"
local obj   = require "obj"
local stats = require"stats"
local the   = lib.settings[[
dostats: example app
(c) 2023 Tim Menzies, BSD-2

USAGE:
  l app101 [OPTIONS]

OPTIONS:
  -f --file  data file               = -
  -h --help  show help text          = false
  -s --seed  set random number seed  = 1234567891]]

local col, SYM, NUM = obj.col, obj.SYM, obj.NUM
local mid, div      = obj.mid, obj.div

local eg={}
function eg.sym(      sym1,m,d)
  sym1 = SYM()
  for _,x in pairs{1,1,1,1,2,2,3} do obj.col(sym1,x) end
  m,d = obj.mid(sym1), obj.div(sym1)
  print(m,d)
  return 1== m and 1.37 <  d and d < 1.38 end

function eg.num(     num1,m,d)
  num1 = NUM()
  for i=1,100 do obj.col(num1, i) end
  m, d = obj.mid(num1), obj.div(num1)
  print(m,d, stats.stdev(obj.has(num1)))
  return 50 == m and m < 51 and 29 < d and d < 32 end

-------------------------------------------------------------
lib.run(the,eg)