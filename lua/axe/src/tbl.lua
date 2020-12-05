-- <img width=75 src="https://cdn0.iconfinder.com/data/icons/data-charts/110/TableDataGridThickLines-512.png">    
-- <a href="http://github.com/tomm/keys"><img src="https://github.blog/wp-content/uploads/2008/12/forkme_left_red_aa0000.png?resize=149%2C149" align=left></a>  
-- "Keys = cluster, discretize, contrast"
-- ![](https://img.shields.io/badge/platform-osx%20,%20linux-lightgrey?style=flat-square)  
-- ![](https://img.shields.io/badge/language-lua,bash-blue?style=flat-square)  
-- ![](https://img.shields.io/badge/purpose-ai%20,%20se-blueviolet?style=flat-square)  
-- ![](https://img.shields.io/badge/language-lua-red?style=flat-square)  
-- ![](https://img.shields.io/badge/license-mit-green?style=flat-square)  
-- [lib](lib.html) :: [tbl](tbl.html)   
--------------------
local Of  = {
  synopois= "tables with rows, summarized in column headers",
  author  = "Tim Menzies, timm@ieee.org",
  license = "MIT",
  year    = 2020,
  seed    = 1,
  ch      = {skip="?", klass="!",sym="_", 
             num=":", more=">", less="<"},
  row     = {p=2,cols="ys"}}

-- ## Objects 
local Lib  = require "lib" 
local Col  = {}
local Num  = {ako="Num", pos=0,txt="",n=0, 
              mu=0, m2=0, sd=0, lo=math.huge, hi= -math.huge}
local Sym  = {ako="Sym", pos=0,txt="",n=0, 
              seen={}, most=0,mode=true}
local Skip = {ako="Skip"}
local Row  = {ako="Row",  cells={}, bins={}}
local Tbl  = {ako="Tbl",  rows={}, cols={},ys={},xs={}, dist={}}

---------------------
-- ## Shortcuts
local isa=Lib.isa
local function cell(x) return not(type(x)=="string" and x=="?") end

---------------------
-- ## Column summaries
function Col.factory(j,s,t) 
  local tmp,aka = Sym, t.xs
  if s:find(Of.ch.num)  then tmp     = Num        end
  if s:find(Of.ch.less) then tmp,aka = Num, t.ys  end
  if s:find(Of.ch.more) then tmp,aka = Num, t.ys  end
  if s:find(Of.ch.sym)  then tmp     = Sym        end
  if s:find(Of.ch.skip) then tmp,aka = Skip,{}    end
  local x = tmp.new(j,s)
  if s:find(Of.ch.klass) then t.class,aka = x,t.ys end 
  t.cols[j] = x
  aka[j]= x end

function Skip.new(n,s) return isa(Skip,{txt=s, pos=n}) end 
function Sym.new(n,s)  return isa(Sym, {txt=s, pos=n}) end
function Num.new(n,s) 
  local  x=isa(Num, {txt=s, pos=n})
  x.w = x.txt:find(Of.ch.less) and -1 or 1
  return x end

function Skip:add(x) return x end 

function Sym:add(x) 
  if cell(x) then
    self.n = self.n + 1
    self.seen[x] = (self.seen[x] or 0) + 1
    if self.seen[x] > self.most then 
      self.most, self.mode = self.seen[x], x end end
  return x end 

function Sym:dist(x,y) return x==y and 0 or 1 end

function Num:add(x) 
  if cell(x) then
    self.n = self.n + 1
    local d = x - self.mu
    self.mu = self.mu + d / self.n
    self.m2 = self.m2 + d*(x - self.mu)
    self.sd = self.m2<0 and 0 or (
              self.n<2  and 0 or (
              (self.m2/(self.n -1))^0.5))
    self.lo = math.min(self.lo,x)
    self.hi = math.max(self.hi,x) end
  return x end

function Num:norm(x) return (x - self.lo) / (self.hi - self.lo + 1E-32) end
function Num:dist(x,y)
  if      not cell(x) then y   = self:norm(y); x=y>0.5 and 0 or 1 
  else if not cell(y) then y   = self:norm(y); y=x>0.5 and 0 or 1 
  else                     x,y = self:norm(x), self:norm(y) end end
  return math.abs(x-y) end 

---------------------
-- ## Row
function Row.new(row,tbl)
  local i = isa(Row)
  for _,col in pairs(tbl.cols) do 
    i.cells[col.pos] = col:add(row[col.pos])
    i.bins[col.pos] = i.cells[col.pos] end 
  return i end

function Row:dist(other,cols,      x,y,d1)
  local d,n,p = 0,1E-32,Of.row.p
  for _,col in pairs(cols) do
    x = self.cells[col.pos]
    y = other.cells[col.pos]
    d1= (not cell(x) and not cell(y)) and 1 or col:dist(x,y)
    d = d+d1^p 
    n = n+1 end
  return (d/n)^(1/p) end

---------------------
-- ## Tables 
function Tbl.new() return isa(Tbl) end

function Tbl:add(t)  
  if #self.cols==0 then 
    for j,x in pairs(t) do Col.factory(j,x,self) end 
  else
    self.rows[(#self.rows)+1] = Row.new(t,self) end end

-- Read from files
function Tbl.read(f,    t) 
  t=Tbl.new()
  for row in Lib.csv(f) do t:add(row) end
  return t end

------
-- And finally...
return {Tbl=Tbl,Row=Row,Sym=Sym,Num=Num}
