#!/usr/bin/env lua
local lib   = require"lib"
local obj   = require"obj"

local the   = lib.settings[[
egstats: generate stats for each class
(c) 2023 Tim Menzies, BSD-2

USAGE:
  eg stats [OPTIONS]

OPTIONS:
  -f --file  data file               = data/diabetes.csv
  -h --help  show help text          = false
  -s --seed  set random number seed  = 1234567891]]

-- ## Names ---------------------------------------------------
local SYM, NUM, DATA,data = obj.SYM, obj.NUM, obj.DATA,obj.data
local csv, oo,report,run  = lib.csv, lib.oo,lib.report,lib.run
local clone,div,mid       = obj.clone, obj.div, obj.mid
local stats = obj.stats

-- ## Examples -------------------------------------------------
local eg = {}
function eg.fail() return false end

function eg.datas(data1,   datas,rows1,all,k,mids,divs)
  datas = {}
  for n,t in csv(the.file) do
    if n==0 then all = DATA({t}) else
      k = t[all.cols.klass.at]
      datas[k] = datas[k] or clone(all)
      data(datas[k], t) end end 
  mids={}
  divs={}
  for k,data1 in pairs(datas) do 
    mids[k] = stats(data1, "x", obj.mid) 
    divs[k] = stats(data1, "x", obj.div,2) end
  report(mids,"\nmid"   ,8) 
  report(divs,"\ndiv",8) end

-- ## main ---------------------------------------------------
run(the,eg)
