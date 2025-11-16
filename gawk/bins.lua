#!/usr/bin/env lua
-- (c) 2025, Tim Menzies, MIT license
-- Naming: t=table, s=string, n=number, val=s or n or "?"
--         row=list[val]; col=NUM or SYM, num=NUM
--         The=config, My=data, UPPERCASE=constructors
local My
local The={bins=7, seed=937162211, pause=25} -- config

-- batteries ------------------------------------------------------------------
local push, cells, csv, coerce, shuffle, pause

function push(t,val) --> val
  t[1+#t]=val; return val end

function coerce(s) --> num or s
  return tonumber(s) or s:match'^%s*(.*%S)' end

function shuffle(t,    i, j)
  for i = #t, 2, -1 do
    j = math.random(i)
    t[i], t[j] = t[j], t[i] end
  return t end

function pause(s,    tty) --> nil
  io.write("\n"..(s or "")..": press enter...")
  tty = io.open("/dev/tty", "r")
  tty:read()
  tty:close() end

function cells(s1,    t) --> t
  t={}; for s2 in s1:gmatch("([^,]+)") do push(t, coerce(s2)) end
  return t end

function csv(file,    src) --> iterator
  src = io.input(file)
  return function(    s)
    s = io.read()
    if s then return cells(s) else io.close(src) end end end 

-- constructors ---------------------------------------------------------------
local SYM, NUM, DATA

function DATA() --> DATA
  return {x={},       -- independent columns
          y={},       -- dependent columns (goals)
          all={},     -- x + y combined (for iteration)
          n=0,        -- total rows seen
          b=0,        -- best rows count
          dist=NUM()} end -- distance tracker

function SYM(n, is) --> SYM
  return {i=n or 0, is=is or "", has={}} end

function NUM(n, is) --> NUM
  return {i=n or 0, is=is or "", n=0, mu=0, sd=0, m2=0, num=true, has={},
          best = (is or ""):find"-$" and 0 or 1} end

-- reasoning ------------------------------------------------------------------
local welford, norm, bin, disty, count, score

function welford(num, val,    d) --> val
  if val == "?" then return val end
  num.n = num.n + 1
  d = val - num.mu
  num.mu = num.mu + d / num.n
  num.m2 = num.m2 + d * (val - num.mu)
  num.sd = num.n < 2 and 0 or math.sqrt(num.m2 / (num.n - 1))
  return val end

function norm(num, val) --> 0..1
  return 1 / (1 + math.exp(-1.7 * (val - num.mu) / (num.sd + 1e-32))) end

function bin(col, val) --> 0..The.bins-1 or val
  if val == "?" then return val end
  if not col.num then return val end
  return math.floor(The.bins * norm(col, val)) end

function disty(row,    d, n, val) --> 0..1
  d, n = 0, 0
  for _, col in ipairs(My.y) do
    val = row[col.i]
    if val ~= "?" then 
      n = n + 1
      d = d + (norm(col, val) - col.best)^2 end end
  return math.sqrt(d / n) end

function count(row,    y, yn, best, val) --> nil
  y = welford(My.dist, disty(row))
  yn = norm(My.dist, y)
  My.n = My.n + 1
  best = yn <= math.sqrt(My.n) / My.n  
  if best then My.b = My.b + 1 end
  for _, col in ipairs(My.x) do
    val = bin(col, row[col.i])
    col.has[val] = col.has[val] or {[true]=0, [false]=0}
    col.has[val][best] = col.has[val][best] + 1 end end

function score(    tmp, val, freqs, b, r) --> tmp
  tmp = {}
  for _, col in ipairs(My.x) do
    for val, freqs in pairs(col.has) do
      b = freqs[true] / My.b
      r = freqs[false] / (My.n - My.b)
      push(tmp, {score=b-r, col=col.is, val=val}) end end
  table.sort(tmp, function(a,b) return a.score > b.score end)
  return tmp end

-- presentation ----------------------------------------------------------------
local sortBins, spark, sparklines

function sortBins(bins) --> bins
  table.sort(bins, function(a,b)
    return a=="?" and false or b=="?" and true or tonumber(a) < tonumber(b) end)
  return bins end

function spark(scores, bins,    max, s, i, chars) --> s
  chars = {" ","▁","▂","▃","▄","▅","▆","▇"}
  max = 0
  for _, val in ipairs(bins) do max = math.max(max, math.abs(scores[val])) end
  s = ""
  for _, val in ipairs(bins) do
    i = math.floor(math.abs(scores[val]) / (max + 1e-32) * 7) + 1
    s = s .. chars[i] end
  return s end

function sparklines(tmp,    cols, bins, s, pad)
  io.write("\027[2J\027[H")
  cols = {}
  for _, one in ipairs(tmp) do
    if not cols[one.col] then cols[one.col] = {} end
    cols[one.col][one.val] = one.score end
  for _, col in ipairs(My.x) do
    if cols[col.is] then
      bins = {}
      for val in pairs(cols[col.is]) do push(bins, val) end
      sortBins(bins)
      s = spark(cols[col.is], bins)
      pad = string.rep(" ", 7 - #bins)  -- pad to align at 7 bins
      io.write(string.format("%-8s %s%s ", col.is .. ":", s, pad))
      for _, val in ipairs(bins) do
        io.write(string.format("%3s ", tostring(val))) end
      io.write("\n") end end 
  pause(My.n) end

-- main -----------------------------------------------------------------------
local header, body, main

function header(row,    col) --> nil
  My = DATA()
  for c, word in ipairs(row) do
    col = (word:match("^[A-Z]") and NUM or SYM)(c, word)
    if not word:match("X[+-]?$") then
      push(word:find"[+-]$" and My.y or My.x, col) end
    push(My.all, col) end end

function body(row) --> nil
  for _, col in ipairs(My.all) do
    if col.num then welford(col, row[col.i]) end end
  count(row)
  if My.n % The.pause == 0 then sparklines(score()) end end

function main(file,    first, rows)
  first = true
  rows = {}
  for row in csv(file or arg[1]) do
    if first then 
      header(row)
      first = false
    else 
      push(rows, row) end end
  math.randomseed(The.seed or os.time())
  shuffle(rows)
  for _, row in ipairs(rows) do
    body(row) end
  sparklines(score()) end

main()
