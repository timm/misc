--<!-- vim: set syntax=lua ts=3 sw=3 et : -->
local l   = {}
local lib = require"lib"
local the = {bins=7, cohen=.33, file=""}

function l.DATA(src,    data1)
  data1 = {rows={}, xnums={}, xcuts={}}
  lib.csv(src, function(nr,s) l.data(data1,nr,s) end)
  for k,xnums in pairs(data1.xnums) do
    data1.xcuts[k] = l.cuts(xnums) end
  return data1 end

function l.data(data1,nr,s)
  if nr==0 then
    print(s)
    for k,v in pairs(lib.cells(s)) do
      if  v:find"^[A-Z]" then
        if not v:find"[+!-]$" then
          data1.xnums[k] = {} end end end
  else
    data1.rows[nr] = {}
    for k,v in pairs(lib.cells(s)) do
      data1.rows[nr][k] = v
      if data1.xnums[k] then
        if v ~="?" then lib.push(data1.xnums[k],v) end end end end end
     
function l.cuts(t,     width,small,i,lo,out,x)
  table.sort(t)
  width = (#t/the.bins//1)
  small = lib.div(t)*the.cohen
  i,lo,out  = width, t[1], {t[1]}
  while i < #t - width do
    x = t[i]
    if x ~= t[i+1] and (x-lo) > small then
      lib.push(out,x)
      lo = t[i+1]
      i  = i + width
    else
      i  = i + 1 end end
  lib.push(out,t[#t])
  return out end

function l.bin(x,cuts,   out)
  if x=="?" then return x end
  if #cuts==1 then return cuts[1] end
  for k=2, #cuts do
    below=cuts[k-1]
    top=cuts[k]
    if x >= below and x <= top then return top end end end 

function l.main(      data1)
  the = lib.cli(the)
  data1 = l.DATA(the.file)
  for _,t in pairs(data1.rows) do
    for k,xcuts1 in pairs(data1.xcuts) do
      t[k] = l.bin(t[k], xcuts1) end 
    print(lib.cat(t)) end 
  end

l.main()