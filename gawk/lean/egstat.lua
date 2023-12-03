#!/usr/bin/env lua
local lib   = require"lib"
local obj   = require"obj"

local the   = lib.settings[[
egstat: count stats over all data
(c) 2023 Tim Menzies, BSD-2

USAGE:
  eg stat [OPTIONS]

OPTIONS:
  -f --file  data file               = data/auto93.csv
  -h --help  show help text          = false
  -s --seed  set random number seed  = 1234567891]]

-- ## Names ---------------------------------------------------
local SYM, NUM, DATA       = obj.SYM, obj.NUM, obj.DATA
local csv,o,oo,run,runall  = lib.csv, lib.o, lib.oo,lib.run,lib.runall
local col, div, mid, stats = obj.col, obj.div, obj.mid,obj.stats

-- ## Examples -------------------------------------------------
local eg  = {}
function eg.fail() return false end

function eg.sym(      sym1,m,d)
  sym1 = SYM()
  for _,x in pairs{1,1,1,1,2,2,3} do col(sym1,x) end
  m,d = mid(sym1), div(sym1)
  print(m,d)
  return 1== m and 1.37 <  d and d < 1.38 end

function eg.num(     num1,m,d)
  num1 = NUM()
  for i=1,100 do col(num1, i) end
  m, d = mid(num1), div(num1)
  print(m,d)
  return 50 < m and m < 51 and 29 < d and d < 30 end

function eg.csv()
  print""
  for n,t in csv(the.file) do if (n% 75) ==0 then oo(t) end end end

function eg.data(data1)
  oo(stats(DATA(the.file),"y",mid,2)) end


-- ## main ---------------------------------------------------
run(the,eg)
