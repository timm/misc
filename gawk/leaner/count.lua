--<!-- vim: set syntax=lua ts=.  3 sw=3 et : -->
-- This code reads data files which names columns on line1:
--
-- - Upper case names indicate numerics (and all else are symbols).
-- - Names ending the `!` are klass columns (there should be only one)
-- - Names ending with `+` or `-` are goals to be maximized, mimizied
--   (there can be many)
-- - Klass and goal columns are the dependent `y` columns.
-- - Everything else are the dependent `x` columns.
--    
-- e.g. for  `Age,job,Salary+`:
--     
--    - `Age` and `Salary` are numeric
--    - `Salary` is a goal to be maximized 
--    - `Age` and `job` are the `x` independent variables.
--     
-- Type hints (for function args, not for locals)
--     
-- - `x` is anything
-- - `n` = number
-- - `s` = string
-- - `xs` = list of many `x`
-- - `t` = table.
-- - `a` = array (index 1,2,3..)
-- - `h` = hash (indexed by keys)
-- - `function XXX()` is a constructor.   
-- - `xxx1` is an instance of `XXX`.
-- - `function xxx(xxx1,...)` updates `xxx1` of type `XXX`.   
--   
-- In function args:
--   
-- - two spaces denotes "start of optionals"
-- - four spaces denotes "start of locals"
local l   = {}
local lib = require"lib"
local the = {file=""}

-- ## Create one column
function l.NUM(at,txt) 
  return {at=at, txt=txt, n=0, has={},
          isSorted=true,
          heaven= (txt or ""):find"-$" and 0 or 1} end

function l.SYM(at,txt) 
  return {at=at, txt=txt, n=0, has={},
          mode=nil, most=0, isSym=true} end

function l.COL(at,txt)
  return ((txt or ""):find"^[A-Z]" and l.NUM or l.SYM)(at,txt) end

-- ## Update one column
function l.col(col1,x)
  return (col1.isSym  and l.sym or l.num)(col1,x) end

-- Update a SYM
function l.sym(sym1,x)
  if x~="?" then
    sym1.n = sym1.n + 1
    sym1.has[x] = 1 + (sym1.has[x] or 0)
    if sym1.has[x] > sym1.most then
      sym1.most, sym1.mode = sym1.has[x],x end end end

-- Update a NUM
function l.num(num1,x)
  if x~="?" then
    num1.n = num1.n + 1
    lib.push(num1.has,x)
    num1.isSorted=false end end 

-- ## Query one column
function l.has(col1)
  if not (col1.isSym or col1.isSorted) then 
    table.sort(col1.has); col1.isSorted=true end
  return col1.has end

function l.mid(col1) 
  return  col1.isSym and col1.mode or lib.mid(l.has(col1)) end

function l.div(col1) 
  return  (col1.isSym and lib.entropy or lib.stdev)(l.has(col1)) end

-- ## Create and update multiple colums
function l.COLS(t, -- e.g. "Age,job,Salary+"    
                x,y,all,klass,col1)  
  x, y, all = {}, {}, {}
  for at, txt in pairs(t) do
    col1 =  l.COL(at,txt)
    lib.push(all, col1)
    if not txt:find"X$" then
      if txt:find"!$" then klass=col1 end
      (txt:find "[-!+]$" and y or x)[at]=col1 end end
  return {klass=klass, names=t, x=x, y=y, all=all} end

-- update a COLS
function l.cols(cols1, t)  
  for _, col1 in pairs(cols1.all) do l.col(col1, t[col1.at]) end end

-- ## Create and update DATA
function l.DATA(src,    data1)
  data1 = {rows={}, cols=nil}
  if src then for t in lib.csv(src) do l.data(data1,t) end end
  return data1 end

function l.data(data1,t)
  if    data1.cols
  then  l.cols(data1.cols, t)
        lib.push(data1.rows, t)
  else  data1.cols= l.COLS(t) end end

-- ## Query   data
function l.clone(data1,rows,     data2)
  data2 = l.DATA()
  data2.cols = l.COLS(data1.cols.names)
  for _,t in pairs(rows or {}) do l.data(data2,t) end
  return data2 end

function l.stats(data1, my,     t) 
  my = lib.defaults(my,{cols="x",ndecs=2,what=l.mid})
  t = {N=#data1.rows}
  for _,col1 in pairs(data1.cols[my.cols]) do
    t[col1.txt] = lib.rnd( my.what(col1), my.ndecs) end
  return t end

-- ## Main
lib.oo(
  l.stats(
    l.DATA(
      lib.cli(the).file)))