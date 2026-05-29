#!/usr/bin/env lua
-- bench.lua <csv>  -- runs fft.lua, prints one summary row
local file = arg[1]
local here = arg[0]:match"(.*/)" or "./"
local cmd  = string.format(
  "/usr/bin/time -p lua %sfft.lua -f %q -t 100 2>&1", here, file)
local h = io.popen(cmd)
local nums, rt = {}, 0
for ln in h:lines() do
  local n = tonumber(ln)
  if n then nums[#nums+1] = n
  else local r = ln:match"^real%s+([%d%.]+)"
       if r then rt = tonumber(r) end end end
h:close()
table.sort(nums)
local function p(k) return nums[math.floor(#nums*k/100)] or 0 end

local fh = io.open(file)
if not fh then return end
local hd = fh:read(); fh:close()
local nx, ny = 0, 0
for raw in hd:gmatch"[^,]+" do
  local t = raw:match"^%s*(.-)%s*$"
  if t:sub(-1) == "X" then
  elseif t:find"[-+!]$" then ny = ny+1
  else nx = nx+1 end end

local nrows = 0
for _ in io.lines(file) do nrows = nrows+1 end
nrows = nrows - 1

local short = file:match"([^/]+)$"
print(string.format("%4d %4d %4d %4d %4d  %4d %4d %6d  %6.2fs  %s",
  p(10),p(30),p(50),p(70),p(90), nx, ny, nrows, rt, short))
