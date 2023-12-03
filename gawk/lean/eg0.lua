#!/usr/bin/env lua
local lib = require "lib"
local the = lib.settings[[
eg0: example app
(c) 2023 Tim Menzies, BSD-2

USAGE:
  eg 0 [OPTIONS]

OPTIONS:
  -f --file  data file               = data/auto93.csv
  -h --help  show help text          = false
  -s --seed  set random number seed  = 1234567891]]

local eg = {}
function eg.fail() return false end
local csv, items,oo, run, runall = lib.csv,lib.items, lib.oo, lib.run, lib.runall
function eg.the() oo(the) end
function eg.csv() for n, t in csv(the.file) do if n%75==0 then oo(t) end end end
function eg.all() runall(the,eg) end

function eg.items() for k,v in items{z=10,a=1,m=4,b=2} do print(k,v) end end

rogueGlobal = 1

run(the,eg)
