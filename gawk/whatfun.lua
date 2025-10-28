#!/usr/bin/env lua
local l = require"lib"
local the = require"config"
local SYM = require("data").SYM
local sqrt, log, abs, pi = math.sqrt, math.log, math.abs, math.pi
local norm, mid, mids, div, entropy, dist, disty, distx, like, likes, _x

function norm(c, v) return v=="?" and v or (v-c.lo) / (c.hi-c.lo+1/the.big) end

-- huh? why mid?
function mid(c,    m, mv)
  m, mv = nil, -math.huge
  l.kap(c.has, function(k, v) if v > mv then m, mv = k, v end end)
  return c.mu or m end

function mids(data) return l.map(data.cols.all, mid) end

function div(c) return c.sd or entropy(c.has) end

function entropy(d,    N, s)
  N = sum(d)
  return - sum(d, function(v) return v/N > 0 and v/N * log(v/N, 2) or 0 end)

function dist(v,    d, n)
  d, n = 0, 0
  for _, x in pairs(v) do n = n + 1; d = d + x ^ the.p end
  return (d / n) ^ (1 / the.p) end

function disty(data, row,      fn)
  fn = function(c) return abs(l.norm(c,row[c.at]) - c.goalp) end
  return dist(l.map(data.cols.y, fn) end

function distx(data, row1, row2,       fn)
  fn = function(c) return _x(c, row1[c.at], row2[c.at]) end)) end
  return dist(l.map(data.cols.x, fn)) end

function _x(col, a, b)
  if a == "?" and b == "?" then return 1 end
  if col.it == SYM then return a ~= b and 1 or 0 end
  a, b = norm(col, a), norm(col, b)
  if a == "?" then a = b > 0.5 and 0 or 1 end
  if b == "?" then b = a > 0.5 and 0 or 1 end
  return math.abs(a - b) end

function like(col, v, prior,    z, var)
  z = 1 / the.big
  if col.it == SYM then
    return log(math.max(z,(col.has[v] or 0 + the.k*(prior or 0))/(col.n+the.k+z))) end
  var = col.sd ^ 2 + z
  return -((v - col.mu) ^ 2 / (2 * var)) - 0.5 * log(2 * pi * var) end

function likes(data, row, nall, nh,    b4, fn)
  nall, nh = nall or 100, nh or 2
  b4 = (data.n + the.m) / (nall + the.m * nh)
  fn = function(x) return row[x.at] ~= "?" and like(x, row[x.at], b4) or 0 end
  return log(b4) + l.sum(data.cols.x, fn) end

return {norm=norm, mids=mids, mid=mid, div=div, entropy=entropy,
        dist=dist, disty=disty, distx=distx, like=like, likes=likes}
