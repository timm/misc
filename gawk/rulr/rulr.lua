#!/usr/bin/env lua
-- <!-- vim: set ts=2 sw=2 sts=2 et: -->
local the={about={what="rulr",
                  why="experiment with fast clustering",
                  who="Tim Menzies",
                  when=2024,
                  copyright="BSD, two clause"}m
           a=1}

local l,b4={},{}; for k,_ in pairs(_ENV) do b4[k]=k end

local DATA,NUM,SYM,COLS={},{},{},{}

function COLS.new(names,     i)
  i = l.is(COLS,{all={}, x={}, y={}, names=names})
  for n,s in pairs(names) do
    push(i.all, (s:find"^[A-Z]" and NUM or SUM)(s,n))
    if not s:find"X$" then push(s:find"[-+!]$" and i.y or i.x, col) end end
  return i end

function DATA.new(it,   i) 
  i = l.is(DATA,{rows={}, cols=COLS.new(it())}) 
  for t in it do i:add(t) end  
  return i end

function Data

-------------------------------------------------------
l.cat = table.concat
l.fmt = string.format

function l.is(x,y) x.__index=x; setmetatable(y,x); return x end

function l.push(t,x) t[1+#t]=x; return x end

function l.items(t,n)
  k,n=0,#t
  function() if k < n then k=k+1; return t[k] end end end

function l.sort(t,fun) table.sort(t,fun); return t end

function l.coerce(s,     other) 
  _other = function(s) if s=="nil" then return nil  end
                       return s=="true" or s ~="false" and s or false end 
  return math.tointeger(s) or tonumber(s) or _other(s:match'^%s*(.*%S)') end

function l.csv(src)
  src = src=="-" and io.stdin or io.input(src)
  return function(      s,t)
    s=io.read()
    if s then
      t={}; for s1 in s:gsub("%s+", ""):gmatch("([^,]+)") do t[1+#t]=l.coerce(s1) end
      return t
    else io.close(src) end end end

### Pretty print

function l.oo(t) print(l.o(t)); return t end

function l.o(t)
 if type(t) ~= "table" then return tostring(t) end
   return (t._name or "") .. "(" .. l.cat((#t==0 and l.dict or l.list)(t,{}) ," ") .. ")" end

function l.list(t,u) 
  for _,v in pairs(t) do u[1+#u] = l.o(v) end ; return u end

function l.dict(t,u) 
  for k,v in pairs(t) do u[1+#u] = l.fmt(":%s %s",k,l.o(v)) end; return l.sort(u) end

function l.rogues() 
  for k,v in pairs(_ENV) do if not b4[k] then print("Rogue?",k,type(v)) end end end
-------------------------------------------------------
local eg={}


eg[arg[1]]()
