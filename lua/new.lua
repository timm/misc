local b4={}; for k, _ in pairs(_ENV) do b4[k]=k end

local the = {
  what = "new.lua",
  why  = "simple inference",
  when = "(c) 2025, MIT License",
  who  = "Tim Menzies",
  rseed = 1234567891, 
  csv   = "../data/auto93m.csv",
  m     = 2, 
  k     = 1 
}

local BIG=1E32

------------------------------------------------------------------------------
local fmt,lt,sort,olist,odict,o,word,words,csv

fmt=string.format

function lt(x) return function(t,u) return t[x] < u[x] end end

function sort(t,fun) table.sort(t,fun); return t end

function olist(x,   t)
  t={}; for _,v in pairs(x) do t[1+#t]=o(v) end; return t end

function odict(x,   t)
  t={}; for k,v in pairs(x) do t[1+#t]=fmt(":%s %s",k,o(v)) end
  return sort(t) end

function o(x)
  return type(x) == "number" and fmt(x//1 == x and "%s" or "%.3g",x) or (
         type(x) ~= "table"  and tostring(x)                         or (
         "{".. table.concat(#x>0 and olist(x) or odict(x)," ") .."}" )) end 

function word(s) return tonumber(s) or s:match("^%s*(.-)%s*$") end

function words(s,   t) 
  t={}; for s1 in s:gmatch"([^,]+)" do t[1+#t]=word(s1) end; return t end

function csv(src,     s,t)
  src = io.input(src)
  s,t = io.read(), {}
  while s do t[1+#t]=words(s); s=io.read() end
  io.close(src) 
  return t end

function push(t,x) t[1+#t] = x; return x end

------------------------------------------------------------------------------
local isNum, isSym, Data

function isNum(s)  return s:find"^[A-Z]" end
function isSym(s)  return not isNum(s) end
function isGoal(s) return s:find"-$" and 0 or 1 end

function Num(txt) return {has={}, ok=true} end

function Sym(txt) return {has={}, counts={}, mode=nil} end

function Data(rows,    i)
  i = {it=Data, rows={}, cols={}}
  for r,row in pairs(rows) do 
    if r>1 then 
      for c,col in pairs(i.cols) do
        if row[c] ~= "?" then
           
function add(v,i,     _num,_sym,_data)
  function _data()
    push(i.rows,v)
    for n,col in pairs(i.cols) do add(v[n], col) end end
  function _num()

    
  if i.it==Data then return _data() end
  if v~="?" then 
    (i.t==Num and _num or _sym)() end 
  return v end

------------------------------------------------------------------------------
local eg={}

eg["--the"]= function(_) print(o(the)) end
eg["--csv"]= function(_) for _,r in pairs(csv(the.csv)) do print(o(r)) end end

if not pcall(debug.getlocal,4,1) then  
  for n,s in pairs(arg) do
    math.randomseed(the.rseed)
    if eg[s] then eg[s](arg[n+1]) else
      for k,_ in pairs(the) do 
        if s=="-"..k:sub(1,1) then the[k]=word(arg[n+1]) end end end end end

for k,v in pairs(_ENV) do if not b4[k] then print("?",k,type(k)) end end 
