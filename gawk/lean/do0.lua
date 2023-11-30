#!/usr/bin/env lua
local lib = require "lib"
local the = lib.settings[[
do1: example app
(c) 2023 Tim Menzies, BSD-2

USAGE:
  l app101 [OPTIONS]

OPTIONS:
  -f --file  data file               = -
  -h --help  show help text          = false
  -s --seed  set random number seed  = 1234567891]]

local eg, oo = {}, lib.oo

function eg.the() oo(the) end
function eg.csv() for _,t in lib.csv(the.file) do oo(t) end end

x=1
lib.run(the,eg)