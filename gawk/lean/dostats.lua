#!/usr/bin/env lua
local lib = require"lib"
local obj = require"obj"

local the = lib.settings[[
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
function eg.sym(s)
  s = SYM()
  for _,x in pairs{1,1,1,1,2,2,3} do col(s,x) end
  print(mid(s), div(s))
  return 1.37 < mid(s) and div(s) < 1.38 end

function eg.num(     n,md,sd)
  n = NUM()
  for i=1,100 do col(n, i) end
  md,sd = mid(n), div(n)
  print(md,sd)
  return 50 < md and md < 51 and 29 < sd and sd < 30 end

-------------------------------------------------------------
lib.run(the,eg)