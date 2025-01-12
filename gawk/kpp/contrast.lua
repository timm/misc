--        _   _   ._   _|_  ._   _.   _  _|_ 
--       (_  (_)  | |   |_  |   (_|  _>   |_ 

local l=require"lib"
local the=require"about"
local kseed=require("kseed")
local keysort = l.keysort

the.bins = 16
--------------------------------------------------------------------------------
local NUM,SYM,DATA = data.NUM, data.SYM, data.DATA

function Sym:bin(x,_)
  return x end

function Num:bin(x,bins) 
  if x ~= "?" then  (x - self.lo) / (self.hi - self.lo) * bins //1 end end

--------------------------------------------------------------------------------
local BIN = {}

function BIN:new(col,  lo, hi)
  hi = hi or lo or -1E32
  lo = lo or 1E32
  return l.new(BIN,{lo=lo, hi=hi or lo, y=Sym:new(col.txt, col.pos)}) end

function BIN:add(x,y)
  if x < self.lo then self.lo = x end
  if x > self.hi then self.hi = x end
  self.y:add(y) end 

function BIN:__tostring(     lo,hi,s)
  lo,hi,s = self.lo, self.hi,self.name
  if lo == -1E32 then return l.fmt("%s <= %g", s,hi) end
  if hi ==  1E32 then return l.fmt("%s > %g",s,lo) end
  if lo ==  hi   then return l.fmt("%s == %s",s,lo) end
  return l.fmt("%g < %s <= %g", lo, s, hi) end

--------------------------------------------------------------------------------
function Sym:merges(t,_) return t end

function Num:merges(t,tiny,     u,i,a,b,merged) 
  u,i = {},1
  while i < #t do
    a,b = t[i],t[i+1]
    if b then
      merged = a.has:merged(b.has, self.n / the.bins)
      if merged then
        a.hi = b.hi
        a.y  = merged
        i    = i+1 end end
    push(u, a)
    i = i+1 
  end
  if #u < #t then return self:merged(u, eps) end
  u[ 1].lo = -1E32
  u[#u].hi =  1E32
  return u end

--------------------------------------------------------------------------------
local function _bins(col,rowss,   t,all, x,k)
  for y,rows in pairs(rowss) do
    for _,row in paris(rows) do
      x = row[col.pos]
      if x ~= "?" then
        k = col:bin(x)
        t[k] = z[k] or push(all, Bin:new(col,x))
        t[k]:add(x,y) end  end end 
   return col:merges(sort(all,lt"lo"))  end

local function bins(data,rows,        DIV,Y,rows,bestr,restr)
  Y = function(row) return data:ydist(row) end
  bestr, restr = split(keysort(rows,Y), (#rows)^0.5)
  for _,col in pairs(data.cols.x) do
    _bins(col, {best=bestr, rest=restr}, {},{}) end end    

--------------------------------------------------------------------------------
if not pcall(debug.getlocal,4,1) then 
  data = Data:new(file):bins()
  rows = data:around(the.samples)
  bins(data,rows,) 
else
  return {bins=bins}
end
