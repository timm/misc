#!/usr/bin/env lua
local lib   = require"lib"
local obj   = require"obj" 
local stats = require"stats"

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
local oo=lib.oo

-- ## Examples -------------------------------------------------
local eg={}

function eg.datas(data1,   datas,rows1,all,k,mids,divs)
  datas = {}
  for n,t in lib.csv(the.file) do
    if n==0 then all = DATA({t}) else
      k = t[all.cols.klass.at]
      datas[k] = datas[k] or obj.clone(all)
      data(datas[k], t) end end 
  mids={}
  divs={}
  for k,data1 in pairs(datas) do 
    mids[k] = obj.stats(data1, data1.cols.x, obj.mid) 
    divs[k] = obj.stats(data1, data1.cols.x, obj.div,2) end
  lib.report(mids,"\nmid"   ,8) 
  lib.report(divs,"\nspread",8) end

-- ## main ---------------------------------------------------
lib.run(the,eg)
