#!/usr/bin/env lua
local help=[[
lite.lua : very simple sequential model optimizer.
Look around a little, learn a little, decide what to do next
(c) 2024 Tim Menzies <timm@ieee.org> BSD 2 clause.
]]
local push,entropy
-------------------------------------------------------
local function settings() return {
  file  = "../../data/auto93.csv",
  seed  = 1234567891,
  magic = {y="[-+!]$", 
           min="-$", 
           num="^[A-Z]"}}as end

local the=settings()
-------------------------------------------------------
local function SYM(name,at)
  return {name=name or "", at=at or 0, n=0, seen={}, most=0, mode=nil} end

local function NUM(name,at)
  return {isNum=true, at=at or 0, name = name or "", n=0,
          heaven=(name or ""):find(the.magic.min) and 0 or 1,
          mu=0,m2=0,lo=1E30, hi=-1E30} end

local function DATA(strs,    all,x,y)
  all,x,y = {},{},{}
  for n,s in pairs(strs) do
    push(all,
      push(s:find(the.magic.y) and y or x,
           (s:find(the.magic.num) and NUM or SYM)(s,n))) end
  return {rows={}, cols={names=strs, x=x, y=y, all=all}} end
-------------------------------------------------------
local function addSym(sym,x)
  sym.seen[x] = 1 + (sym.seen[x] or 0)  end

local function addNum(num,x,     delta)
  num.n  = num.n + 1
  delta  = x - num.mu
  num.mu = num.mu + delta/num.n
  num.m2 = num.m2 + delta*(x - num.mu)
  if x > num.hi then num.hi = x end
  if x < num.lo then num.lo = x end end

local function add(data,t,    x)
  print(data.rows)
  push(data.rows, t)
  for _,col in pairs(data.cols.all) do
    x = t[col.at]
    if x ~= "?" then 
      col.n = col.n + 1
      (col.isNum and addNum or addSym)(col,x) end end end

--------------------------------------------------------------
local div,mid,norm,sd -- norm (defined above)

function div(col)    return col.isNum and sd(col) or entropy(col.has) end
function mid(col)    return col.isNum and col.mu or col.mode end
function norm(num,x) return x=="?"and x or (x - num.lo)/(num.hi - num.lo + 1E-30) end
function sd(num)     return num.n < 2 and 0 or (num.m2/(num.n - 1))^.5 end

function loss(data,t,    d)
  d = 0
  for _,c in pairs(data.cols.y) do d=d + (norm(c, t[c.at]) - c.heaven)^2 end
  return (d/#data.cols.y)^.5 end
----------------------------------------------------------------
-- ## Misc library functions
function push(t,x) t[1+#t]=x; return x end

function entropy(t,   N,e)
  e,N=0,0
  for _,n in pairs(t) do N=N+n end 
  for _,n in pairs(t) do e=e + n/N*math.log(n/N,2) end
  return -e end
----------------------------------------------------------------
-- ## Examples
local eg = {}

local function normal(mu,sd,    R)
  R=math.random
  return (mu or 0) + (sd or 1) * math.sqrt(-2 * math.log(R()))
                               * math.cos(2 * math.pi * R()) end

local function run(x)
  the = settings()
  math.randomseed(the.seed or 1234567891)
  return eg[x]() end

function eg.num(  n)
  n=NUM()
  for _=1,10^4 do addNum(n,normal(10,2)) end
  print(sd(n), mid(n)) end

if  not pcall(debug.getlocal, 4, 1) and arg[1] then run(arg[1]) end