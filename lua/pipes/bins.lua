#!/usr/bin/env lua
l=require"lib"
local the=l.settings[[

bins: discretize

USAGE: ./bins

OPTIONS
  -b --bins number of bins = 5
  -f --file file to load   = 
  -h --help show help      = false]]

the = l.cli(the)

local nums,rows,names = 0,{},{}

local function head(t)
  names=t
  for n,txt in pairs(t) do
    if txt:find"^[A-Z]" then nums[n]={} end end end

local function body(row,    x)
  rows[1+#rows] = row
  for i,num in pairs(nums) do 
    x = row[i]
    if x ~= "?" then num[1+#num] = x end end end 

local function bin(i.v)
  
local todo=head
l.csv(the.file, function(t) todo(t); todo= body end)

for _,num in pairs(nums) do table.sort(num) end

print(table.concat(row,", "))
for _,row  in pairs(rows) do
  print(table.concat(row,", "))
  print(table.concat(map(row,bin),", ")) end
