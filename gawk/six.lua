local int,sqrt,exp = math.tointeger, math.sqrt, math.exp
local fmt = string.format
local cast,csv,kap,map,o,order
local the,help = {}, [[
  -a apple=2.23222 asdas
  -s seed=1 random number seed
  -c card=4 asdas ]]

-- create ----------------------------------------------------------------------
function COL(n,s) return (s:find"^[A-Z]" and NUM or SYM)(n,s) end
function SYM(n,s) return {it=SYM, at=n or 0, txt=s or "", n=0, has={}} end
function NUM(n,s) return {it=NUM, at=n or 0, txt=s or "", n=0, mu=0, m2=0, sd=0, 
                          goal = (s or ""):find"-$" and 0 or 1 } end

function DATA(items) return adds(items, {it=DATA, rows={}, cols=nil, n=0}) end

function COLS(row,     all,isx,isy)
  isx = function(c) return not c.txt:find"[+-X]$" end
  isy = function(c) return c.txt:find"[+-]$" end
  all = kap(row,COL)
  return {it=COLS, names=rows, all=all, x= map(all,isx), y= map(all,isy)} end

-- update ----------------------------------------------------------------------
function add(i,v)
  local function _sym() i.has[v] = 1 + (i.has[v] or 0) end
  local function _num(   d)
    d = v - i.mu; i.mu = i.mu + d/i.n; i.m2 = i.m2 + d*(v - i.mu)
    i.sd = i.n < 2 and 0 or sqrt(i.m2/(i.n - 1)) end
  local function _data()
    if not i.cols then i.cols=COLS(v) else
      i.rows[1 + #i.rows] = row
      for _,col in pairs(i.cols.all) do add(col, row[col.at]) end end end

  if v~="?" then 
    i.n=i.n+1
    (i.it==SYM and _sym or i.it==NUM and _num or i.it==DATA and _data)() end 
  return v end

-- query -----------------------------------------------------------------------
function norm(num,v) return 1 / (1 + exp( -1.7 * (v - num.mu) / num.sd)) end
function val(i,v) return (SYM==i.it or v=="?") and v or int(the.bins*norm(i,v)) end

-- lib ------------------------------------------------------------------------
function adds(  items, i)
  items = items or {}
  i = i or NUM() 
  if type(items) == "function" then for item in items do add(i, item) end
  else for _, item in pairs(items) do add(i, item) end end
  return i end

function map(t,f,   u) u={};for _,s in pairs(t) do u[1+#u]=f(  v)end; return u end
function kap(t,f,   u) u={};for k,s in pairs(t) do u[1+#u]=f(k,v)end; return u end

function cast(s) return int(s) or tonumber(s) or s:match"^%s*(.-)%s*$" end

function csv(file,    src)
  src = assert(io.open(file))
  return function(    s)
    s = src:read(); if s then return cast(s) else src:close() end end end

function order(t,     u,i)
  if #t>0 then return ipairs(t) end; 
  u,i = {},0
  for k in pairs(t) do u[#u+1]=k end; table.sort(u)
  return function() i=i+1; if u[i] then return u[i],t[u[i]] end end end

function o(t,     u)
  if math.type(t) == "float" then return fmt("%.2f",t) end
  if type(t) == "function" then return "()" end
  if type(t) ~= "table" then return tostring(t) end
  u={}; for k,v in order(t) do u[#u+1] = #t>0 and o(v) or fmt(":%s %s",k,o(v)) end
  return "{"..table.concat(u," ").."}" end

-- start-ip --------------------------------------------------------------------
for k,v in help:gmatch("(%S+)=(%S+)") do the[k] = cast(v) end
math.randomseed(the.seed)
print(o(the))
