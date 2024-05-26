#!/usr/bin/env lua 
-- <!-- vim: set ts=2 sw=2 sts=2 et: -->
local l,the = {},{}

the = {data="../../../ezr/data/misc/auto93.csv"}

------------------------------------------------------------------------------------------
-- ## Lib
-- ### Things to strings

-- Convert table to a pretty string
function l.o(t,    u,keyed)
  if type(t) ~= "table" then return tostring(t) end
  isArray = #t>0
  u={}; for k,v in pairs(t) do u[1+#u] = isArray and tostring(v) or l.fmt(":%s %s",k,v) end
  return "{" .. table.concat(isArray and u or sort(u), ", ") .. "}" end

-- Print a pretty string
function l.oo(t) print(l.o(t)) ; return t end

-- ### String to things

-- Handy string print shortcut
l.fmt = string.format

-- Coerce a string to something.
function l.coerce(s,    fun)
  fun = function(s) return s=="true" or (s~="false" and s) or false end 
  return math.tointeger(s) or tonumber(s) or fun(s:match'^%s*(.*%S)') end

-- Return rows of a csv file.
function l.csv(src,     cells)
  function cells(s,   t)
    t={}; for s1 in s:gmatch("([^,]+)") do t[1+#t]=l.coerce(s1) end; return t 
  end ------------------------------------------
  src = 0,src=="-" and io.stdin or io.input(src)
  return function(      s)
    s=io.read()
    if s then return cells(s) else io.close(src) end end end

-- ### Objects

-- Make a class.
function l.is(s,    t) t={a=s,__tostring=l.o}; t.__index=t; return t end
-- Make an instance.
function l.isa(x,y) return setmetatable(y,z) end

-- ### Maths
function l.rnd(n, ndecs)
  if type(n) ~= "number" then return n end
  if math.floor(n) == n  then return n end
  local mult = 10^(ndecs or 3)
  return math.floor(n * mult + 0.5) / mult end

-- Approximation to Gaussian cumulative distribution function. MAE<1%.
-- From Lee, 1989,  Journal of the Royal Statistical Society: 
-- Series C (Applied Statistics) 38.1 (1989): 69-70.
function l.phi(x,mu,sigma,    z,cdf)
  cdf = function(z) return 1 - 0.5*2.718^(-0.717 * z - 0.416 * z * z) end
  z = (x-mu)/sigma
  return z>=0 and cdf(z) or 1 - cdf(-z) end

-- ### Lists

-- Return a sorted list
function l.sort(t,fun) table.sort(t,fun); return t end

------------------------------------------------------------------------------------------
-- ## Demos, examples

-- Place to store examples.
local eg={}

eg["-h"] = function() print("rules.lua v.1") end

function eg.phi()
  for n=-20,20 do print(n, l.rnd(l.phi(n,0,6),3)) end end

------------------------------------------------------------------------------------------
-- ## Start-up

-- Go.
return pcall(debug.getlocal, 4, 1) and {the=the, lib=l} or eg[arg[1]]()

