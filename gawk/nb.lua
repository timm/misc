#!/usr/bin/env lua
local the,help = {}, [[
nb.lua: naive bayes classifier
(c) 2026, Tim Menzies, MIT license.

Options:
  -h                  show helpw
  -k k=1              bayes low frequency hack1
  -m m=2              bayes low frequency hack2
  -s seed=1           random number seed ]]

local int,sqrt,exp,log,max = math.tointeger,math.sqrt,math.exp,math.log,math.max
local fmt = string.format
local BIG=1E32

local SYM,NUM,DATA,COLS = {_is="SYM"},{_is="NUM"},{_is="DATA"},{_is="COLS"}
local Sym,Num,Data,Cols,Col,clone

-- iterators -------------------------------------------------------------------
local iter,order

function iter(t,    more,state,key)
  if type(t)=="function" then return t end
  more,state,key = pairs(t)
  return function(v) key,v = more(state,key); return v end end

function order(t,     u,j)
  if #t>0 then return ipairs(t) end
  u,j = {},0
  for k in pairs(t) do u[#u+1]=k end; table.sort(u)
  return function() j=j+1; if u[j] then return u[j],t[u[j]] end end end

-- lib -------------------------------------------------------------------------
local adds,sum,kap,sel,most,cast,casts,csv,isa,o

function isa(mt,t) mt.__index=mt; return setmetatable(t,mt) end
function cast(s) return int(s) or tonumber(s) or s:match"^%s*(.-)%s*$" end

function sum(t,f,    n) n=0; for _,v in pairs(t) do n=n+f(v) end; return n end
function kap(t,f,    u) u={};for k,v in pairs(t) do u[1+#u]=f(k,v) end;return u end
function sel(t,f,    u) 
  u={};for _,v in pairs(t) do if f(v) then u[1+#u]=v end end;return u end
function most(t,f,   n,out,tmp)
  n = -BIG; for k,v in pairs(t) do
    tmp = f(k,v); if tmp and tmp > n then n,out = tmp,k end end
  return out end

function adds(items,t)
  t=t or Num(); for v in iter(items or {}) do t:add(v) end; return t end

function casts(s,sep,    t)
  t={}; for x in s:gmatch("[^"..sep.."]+") do t[1+#t]=cast(x) end; return t end

function csv(file,    src)
  src = assert(io.open(file))
  return function(s)
    s=src:read(); if s then return casts(s,",") else src:close() end end end

function o(t,     u,mt)
  if math.type(t)=="float" then return fmt("%.2f",t) end
  if type(t)~="table" then return tostring(t) end
  for k,v in order(t) do u[1+#u]=#t>0 and o(v) or fmt(":%s %s",k,o(v)) end
  mt=getmetatable(t); u={}
  return (mt and mt._is or "").."{"..table.concat(u," ").."}" end

-- create ----------------------------------------------------------------------
function Col(n,s) return (s:find"^[A-Z]" and Num or Sym)(n,s) end
function Sym(n,s) return isa(SYM,{at=n or 0, txt=s or "", n=0, has={}}) end
function Num(n,s) return isa(NUM,{at=n or 0, txt=s or "", n=0, mu=0, m2=0, sd=0}) end
function Data(txt,items)
  return adds(items or {}, isa(DATA,{txt=txt or "", rows={}, cols=nil})) end

function Cols(row,    all)
  all = kap(row, Col)
  return isa(COLS, {names=row, all=all,
    x=sel(all, function(c) return not c.txt:find"[+!X-]$" end),
    y=sel(all, function(c) return c.txt:find"[+!-]$" end)}) end

function clone(data,rows) return adds(rows, Data(data.txt, {data.cols.names})) end

-- methods ----------------------------------------------------------------------
function SYM.add(i,v)
  if v~="?" then i.n=i.n+1; i.has[v]=1+(i.has[v] or 0) end end

function NUM.add(i,v,    d)
  if v~="?" then
    i.n=i.n+1; d=v-i.mu; i.mu=i.mu+d/i.n; i.m2=i.m2+d*(v-i.mu)
    i.sd = i.n<2 and 0 or sqrt(i.m2/(i.n-1)) end end

function DATA.add(i,row)
  if not i.cols then i.cols=Cols(row) else
    i.rows[1+#i.rows] = row
    for _,col in pairs(i.cols.all) do col:add(row[col.at]) end end end

function SYM.like(i,v,prior,    n)
  n = (i.has[v] or 0) + the.k*(prior or 0)
  return max(1/BIG, n/(i.n + the.k + 1/BIG)) end

function NUM.like(i,v,    z,var)
  z=1/BIG; var=i.sd^2 + z
  return (1/sqrt(2*math.pi*var)) * exp(-((v - i.mu)^2)/(2*var)) end

function DATA.likes(i,row,nall,nh,    b4)
  b4 = (#i.rows + the.m)/(nall + the.m*nh)
  return log(b4) + sum(i.cols.x, function(c)
    return row[c.at]~="?" and log(c:like(row[c.at],b4)) or 0 end) end

-- classify --------------------------------------------------------------------
local function nb(items,    all,klasses,n,nk,klass,train,seen,classify)
  klasses, n, nk = {}, 0, 0
  function klass(row) return row[all.cols.y[1].at] end
  function train(row) klasses[klass(row)]:add(row) end
  function seen(k)
    if not klasses[k] then 
      nk=nk+1; klasses[k]=clone(all); klasses[k].txt=k end end
  function classify(row,      guess)
    function guess(_,d) return #d.rows>0 and d:likes(row,n,nk) end
    return most(klasses,guess) end

  for row in iter(items) do
    if not all then all=Data("all",{row}) else
      seen(klass(row))
      if n > 5 then print(classify(row), klass(row)) end
      n=n+1; train(row) end end end

-- demos -----------------------------------------------------------------------
local eg={}

eg["-h"]     = function(_) print("\n"..help) end

eg["--order"]= function(_) for k,v in order({z=1,a=2,m=3}) do print(k,v) end end
eg["--iter"] = function(_,    t,f)
  t={}; for x in iter({10,20,30}) do t[1+#t]=x end; print(o(t))
  t={}; f=function() local i=0; return function() i=i+1; if i<4 then return i*10 end end end
  for x in iter(f()) do t[1+#t]=x end; print(o(t)) end

eg["--the"]  = function(_) print(o(the)) end

eg["--sym"]  = function(_) print(o(adds({"a","a","a","b","c"},Sym()))) end
eg["--num"]  = function(_) print(o(adds({10,20,30,40}))) end
eg["--col"]  = function(_) print(o(Col(1,"Age")), o(Col(2,"name"))) end
eg["--cols"] = function(_) print(o(Cols({"Name","Age","Weight-","Class!"}).y)) end

eg["--csv"]  = function(f) for row in csv(f) do print(o(row)) end end
eg["--data"] = function(f) print(o(Data("",csv(f)).cols.y[1])) end

eg["--like"] = function(_,    num,sym)
  num=adds({10,20,30,40,50}); sym=adds({"a","a","a","b","c"},Sym())
  print(num:like(30), sym:like("a",0.5)) end

eg["--likes"]= function(f,    data)
  data=Data("",csv(f)); print(data:likes(data.rows[1], #data.rows, 2)) end

eg["--nb"]   = function(f) nb(csv(f)) end

for k,v in help:gmatch("(%S+)=(%S+)") do the[k]=cast(v) end
if arg[0] and arg[0]:find"nb" then
  for j,s in pairs(arg) do if eg[s] then eg[s](arg[j+1]) end end end

return {the=the, BIG=BIG, SYM=SYM, NUM=NUM, DATA=DATA, COLS=COLS,
        Sym=Sym, Num=Num, Data=Data, Cols=Cols, Col=Col, clone=clone,
        iter=iter, order=order, adds=adds, sum=sum, kap=kap,
        sel=sel, most=most, cast=cast, casts=casts, csv=csv, isa=isa,
        o=o,  nb=nb, eg=eg}
