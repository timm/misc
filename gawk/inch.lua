local the,help = {}, [[
inch.lua: incremental optimization in lua
(c) 2026, Tim Menzies, MIT license.

Options:
  -b bins=10
  -k k=1
  -m m=2
  -s seed=1           random number seed
  -t threhsold=-1.28  what is 'good' ]]

local int,sqrt,exp,log,max = math.tointeger,math.sqrt,math.exp,math.log,math.max
local fmt = string.format
local BIG=1E32

-- iterators -------------------------------------------------------------------
local iter,order

function iter(t,    nxt,state,key)
  if type(t)=="function" then return t end
  more,state,key = pairs(t)
  return function(v) key,v = more(state,key); return v end end

function order(t,     u,i)
  if #t>0 then return ipairs(t) end; 
  u,i = {},0
  for k in pairs(t) do u[#u+1]=k end; table.sort(u)
  return function() i=i+1; if u[i] then return u[i],t[u[i]] end end end

-- lib ------------------------------------------------------------------------
local adds, sum, map, kap, cast, casts, csv, o, run, runs

function adds(items,i) 
  i=i or Num(); for item in iter(items or {}) do add(i,item) end;return i end

function sum(t,f,   n) n=0;for _,v in pairs(t)do n=n+f(v) end; return n end
function map(t,f,   u) u={};for _,v in pairs(t)do u[1+#u]=f(  v)end;return u end
function kap(t,f,   u) u={};for k,v in pairs(t)do u[1+#u]=f(k,v)end;return u end

function cast(s) return int(s) or tonumber(s) or s:match"^%s*(.-)%s*$" end

function casts(s,sep,    t) 
  t={};for x in s:gmatch("[^"..sep.."]+")do t[1+#t]=cast(x) end;return t end

function csv(file,    src)
  src = assert(io.open(file))
  return function(    s)
    s = src:read(); if s then return casts(s,",") else src:close() end end end

function o(t,     u)
  if math.type(t) == "float" then return fmt("%.2f",t) end
  if type(t) ~= "table" then return tostring(t) end
  u={};for k,v in order(t) do u[1+#u]=#t>0 and o(v) or fmt(":%s %s",k,o(v)) end
  return "{"..table.concat(u," ").."}" end

function run(f,v) if f then math.randomseed(the.seed); f(v) end end

function runs(funs) 
  for i,s in pairs(arg) do run(funs[s],arg[i+1]) end end

-- create ----------------------------------------------------------------------
local      SYM, NUM, DATA, COLS = "SYM", "NUM", "DATA", "COLS"
local Col, Sym, Num, Data, Cols

function Col(n,s) return (s:find"^[A-Z]" and Num or Sym)(n,s) end
function Sym(n,s) return {it=SYM, at=n or 0, txt=s or "", n=0, has={}} end
function Num(n,s) return {it=NUM, at=n or 0, txt=s or "", n=0, mu=0, m2=0, sd=0,
                          goal = (s or ""):find"-$" and 0 or 1} end

function Data(items) return adds(items, {it=DATA, rows={}, cols=nil, n=0}) end

function Cols(row,     all,isx,isy)
  isx = function(c) return not c.txt:find"[+-X]$" end
  isy = function(c) return c.txt:find"[+-]$" end
  all = kap(row, Col)
  return {it=COLS, names=row, all=all, x=map(all,isx), y=map(all,isy)} end

-- update ----------------------------------------------------------------------
local add, norm, val, like, likes

function add(i,v,  d)
  if v~="?" then
    i.n = i.n + 1
    if SYM==i.it then 
      i.has[v] = 1 + (i.has[v] or 0)
    elseif NUM==i.it then 
      d = v - i.mu; i.mu = i.mu + d/i.n; i.m2 = i.m2 + d*(v - i.mu)
      i.sd = i.n < 2 and 0 or sqrt(i.m2/(i.n - 1))
    elseif DATA==i.it then 
      if not i.cols then i.cols=Cols(v) else
        i.rows[1 + #i.rows] = v
        for _,col in pairs(i.cols.all) do add(col, v[col.at]) end end end end
  return v end

function norm(num,v) 
  return 1 / (1 + exp( -1.7 * (v - num.mu) / num.sd)) end

function val(i,v) 
  return (SYM==i.it or v=="?") and v or int(the.bins*norm(i,v)) end

function like(col,v,prior,     z,var,n)
  z = 1 / the.big
  if SYM == col.it then 
    n = (col.has[v] or 0) + the.k*(prior or 0)
    return max(z, n / (col.n + the.k + z)) 
  else
    var = col.sd^2 + z
    return (1 / sqrt(2*math.pi*var)) * exp(-((v - col.mu)^2) / (2*var)) end end

function likes(data,row,nall,nh,     b4,fn)
  nall,nh = nall or 100,nh or 2
  b4 = (data.n+the.m) / (nall+the.m*nh)
  fn = function(x) return row[x.at]~="?" and log(like(x,row[x.at],b4)) or 0 end
  return log(b4) + sum(data.cols.x, fn) end

-- demos -----------------------------------------------------------------------
local eg = {}

eg["-h"]= function(_) print("\n"..help) end

eg["--the"]= function(_) print(o(the)) end

for k,v in help:gmatch("(%S+)=(%S+)") do the[k] = cast(v) end
runs(eg)
