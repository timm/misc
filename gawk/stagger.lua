#!/usr/bin/env lua
-- Fully incremental mulit-objective XAI.    
-- (c) 2025 Tim Menzies. MIT License:
-- https://opensource.org/licenses/MIT
--    
-- For required argumends in each function,
-- this code uses short, consistent names for primitive arguments:
-- `t` (table), `s` (string), `n` (number), `val` (string/number/?),
-- and `row`/`rows` for lists of values.
--    
-- Also, schema names describe column roles: `col` is a NUM or SYM column,
-- `num` is numeric-only, `bs` is a list of bins, and `cols` maps
-- columns to their bin-score tables.
--      
-- Finally, context variables track system state: `The` holds configuration,
-- `My` is the active DATA object, and DATA/NUM/SYM are constructors
-- defining table, numeric, and symbolic column types.
local My; local The = {bins = 4,seed = 937162211,pause = 25}

--## Batteries
local push,cells,csv,coerce,shuffle,pause

-- Add a value to a table.
function push(t,val) --> val
  t[#t+1] = val; return val end

-- Convert a string to number if possible.
function coerce(s) --> n or s
  return tonumber(s) or s:match'^%s*(.*%S)' end

-- Shuffle a table in-place.
function shuffle(t,j) --> t shuffled
  for i = #t,2,-1 do j = math.random(i);t[i],t[j] = t[j],t[i] end
  return t end

-- Pause execution and wait for keypress.
function pause(s,    tty) --> nil
  io.write("\n"..(s or "")..": press enter...")
  tty  =  io.open("/dev/tty", "r")
  tty:read()
  tty:close() end

-- Split comma-separated string into cells.
function cells(s1,t) --> t
  t = {}; for s2 in s1:gmatch"([^,]+)" do push(t,coerce(s2)) end
  return t end

-- Return an iterator over CSV rows.
function csv(file,src) --> iterator
  src = io.open(file,"r")
  return function()
    local s  =  src:read()
    if s then return cells(s) end
    src:close()
    return nil end end

--## Constructors 
local SYM,NUM,DATA

-- Create a new DATA object.
function DATA() --> DATA
  return {x={},y = {},all = {},n = 0,b = 0,dist = NUM()} end

-- Create a new SYM column.
function SYM(n,is) --> SYM
  return {i=n or 0,is = is or"",has = {}} end

-- Create a new NUM column.
function NUM(n,is) --> NUM
  return {i=n or 0,is = is or"",num = true,n = 0,mu = 0,sd = 0,m2 = 0,has = {},
          best=(is or""):find"-$" and 0 or 1} end

--## Reasoning 

-- Update numeric stats using Welford’s algorithm.
function welford(num,val,d) --> val
  if val == "?" then return val end
  num.n = num.n+1
  d = val-num.mu
  num.mu = num.mu+d/num.n
  num.m2 = num.m2+d*(val-num.mu)
  num.sd = num.n<2 and 0 or math.sqrt(num.m2/(num.n-1))
  return val end

-- Normalize a numeric value to 0..1.
function norm(num,val) --> 0..1
  return 1/(1+math.exp(-1.7*(val-num.mu)/(num.sd+1e-32))) end

-- Map numeric value to a bin.
function bin(col,val) --> 0..The.bins-1 or val
  return (val == "?" or not col.num) and val
     or math.floor(The.bins*norm(col,val)) end

-- Compute distance-to-best for a row.
function disty(row,d,n,val) --> 0..1
  d,n = 0,0
  for _,col in ipairs(My.y) do
    val = row[col.i]
    if val ~= "?" then
      n = n+1; d = d+(norm(col,val)-col.best)^2
    end end
  return math.sqrt(d/n) end

-- Update best/rest counts and feature bins.
function count(row,     y,ny,isBest,val) --> nil
  y = welford(My.dist,disty(row))
  ny = norm(My.dist,y)
  My.n = My.n+1
  isBest = ny <= math.sqrt(My.n)/My.n
  if isBest then My.b = My.b+1 end
  for _,col in ipairs(My.x) do
    val = bin(col,row[col.i])
    col.has[val] = col.has[val] or {[true] = 0,[false] = 0}
    col.has[val][isBest] = col.has[val][isBest]+1
  end end

-- Score bins by separation of best vs rest.
function score(tmp,bad,good) --> tmp
  tmp = {}
  for _,col in ipairs(My.x) do
    for val,f in pairs(col.has) do
      good = f[true]/My.b
      bad  = f[false]/(My.n-My.b)
      push(tmp,{score = good-bad,col = col.is,val = val})
    end end
  table.sort(tmp,function(a,b) return a.score>b.score end)
  return tmp end

--## Presentation 
local sortBins,spark,sparklines

-- Sort bins numerically.
function sortBins(bs) --> sorted bs
  table.sort(bs,function(a,b)
    return a == "?" and false or
           b == "?" and true or
           tonumber(a)<tonumber(b)
  end)
  return bs end

local bars = {" ","▁","▂","▃","▄","▅","▆","▇"}
local cols = {
  "\027[38;5;88m","\027[38;5;124m","\027[38;5;166m",
  "\027[38;5;220m","\027[38;5;190m","\027[38;5;148m",
  "\027[38;5;40m","\027[38;5;46m"
}

-- Render a single sparkline.
function spark(cs,bs,glob,s,i) --> sparkline string with color
  s = ""
  for _,b in ipairs(bs) do
    i = math.floor((math.abs(cs[b])/(glob+1e-32))*7)+1
    s = s..cols[i]..bars[i]
  end
  return s.."\027[0m" end

local BARW = 10

-- Print colored sparklines for all bins.
function sparklines(tmp,cols,glob,bs) --> nil
  io.write("\027[2J\027[H")
  cols,glob  =  {},0
  for _,one in ipairs(tmp) do
    cols[one.col] = cols[one.col] or{}
    cols[one.col][one.val] = one.score
    glob = math.max(glob,math.abs(one.score)) end
  for _,col in ipairs(My.x) do
    local cs = cols[col.is]
    if cs then
      bs = {}; for b in pairs(cs) do push(bs,b) end
      table.sort(bs)
      local s = spark(cs,bs,glob)
      local pad = string.rep(" ",BARW-#bs)
      io.write(("%20s %s%s "):format(col.is..":",s,pad))
      for _,b in ipairs(bs) do io.write(b.." ") end
      io.write("\n")
    end end
  pause(My.n) end

--## Main 
local header,body,main

-- Initialize DATA from header row.
function header(row,col) --> nil
  My = DATA()
  for i,word in ipairs(row) do
    col = (word:match"^[A-Z]" and NUM or SYM)(i,word)
    if not word:match"X[+-]?$" then
      push(word:find"[+-]$" and My.y or My.x,col) end
    push(My.all,col) end end

-- Process one data row.
function body(row) --> nil
  for _,col in ipairs(My.all) do
    if col.num then welford(col,row[col.i]) end end
  count(row)
  if My.n%The.pause == 0 then sparklines(score()) end end

-- Main driver for reading, shuffling, and processing.
function main(file,first,rows) --> nil
  first = true; rows = {}
  for row in csv(file or arg[1]) do
    if first then header(row); first = false
    else push(rows,row) end end
  math.randomseed(The.seed or os.time()); shuffle(rows)
  for _,row in ipairs(rows) do body(row) end
  sparklines(score()) end

main()

--## How to Contribute to This Code
--
-- This file follows a specific compact Lua coding style.  
-- When modifying or adding code, please follow all rules below.
--
--### 1. One-Line Function Comments
-- Place **one comment line directly above every function**.
-- Requirements:
--   - Starts with uppercase
--   - Ends with a period `.`
--   - Short (3–8 words)
-- Example:
--   -- Update numeric statistics.
--   function welford(num,val) ...
--
--### 2. Naming Conventions
-- Use short, consistent argument names:
--   t=table, s=string, n=number, val=s|n|"?"
--   row=list[val], rows=list[row]
--   col=NUM|SYM, num=NUM, bs=bins-list
-- Constructors are uppercase: DATA, NUM, SYM.
-- Utilities are lowercase: push, csv, cells, shuffle, pause.
--
--### 3. Compact, Minimal Formatting
-- Keep functions small and elegant.
-- Avoid blank lines unless separating sections.
-- Prefer expressions over statements when clear.
-- Keep lines reasonably short.
--
--### 4. Comment and Section Style
-- Section headers use:
--   --## section name
-- Do not write long essays in comments.
-- Keep all comments concise and functional.
--
--### 5. Table/Object Shape
-- Return tables in compact one-line form when possible:
--   return {i=i or 0, is=is or "", n=0, mu=0, sd=0}
--
--### 6. Bin and Count Structures
-- Use simple nested tables for best/rest counts:
--   col.has[val] = col.has[val] or {[true]=0, [false]=0}
--
--### 7. Function Signatures
-- Always include a compact return-type hint:
--   function foo(x,y) --> nil
-- Use short hints such as: --> t, --> val, --> 0..1, --> iterator
--
--### 8. Iterator Pattern
-- All iterators should follow the closure style:
--   return function()
--     local s = src:read()
--     if s then return cells(s) end
--     src:close()
--     return nil
--   end
--
--### 9. Randomness + Determinism
-- Use:
--   math.randomseed(The.seed or os.time())
-- Shuffle using the supplied `shuffle()` utility.
--
--### 10. Output Style
-- Use io.write instead of print.
-- Align columns with formatted strings when needed.
--
--### 111. Overall Aesthetic
-- Aim for: compact, clean, deterministic, functional, elegant.
-- No OOP frameworks. No metatables unless essential.
-- Fewer globals, more clarity.
