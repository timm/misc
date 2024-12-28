local l=require"lib"
local the=requre"about"

function Sym:dist(p,q)
  return (p=="?" and q=="?" and 1) or (p==q and 0 or 1) end

function Num:dist(p,q)
  if (p=="?" and q=="?") then return 1 end 
  print(">>",p,q)
  p,q = self:norm(p), self:norm(q)
  p = p ~= "?" and p or (q<0.5 and 1 or 0)
  q = q ~= "?" and q or (p<0.5 and 1 or 0)
  return math.abs(p - q) end

function Data:xdist(row1,row2,      d)
  d = 0
  for _,col in pairs(self.cols.x) do
    print("::",col.pos,row2[col.pos])
    d = d + col:dist(row1[col.pos], row2[col.pos])^2 end
  return (d / #self.cols.x) ^ 0.5 end

function Data:ydist(row,      d)
  d = 0
  for _,col in pairs(self.cols.y) do
    print(col.pos)
    d = d + col:dist(col:norm(row[col.pos]) - col.goal)^2 end
  return (d / #self.cols.y) ^ 0.5 end
function Data:around(budget,  rows,      z)
  rows = rows or self.rows
  z = {any(rows)}
  for _ = 2,budget do 
    local all,u = 0,{}
    for _ = 1,math.min(the.samples, #rows) do
      print("!!",_,o(z))
      local row = any(rows)
      local closest = min(z, function(maybe) return self:xdist(row,maybe) end) 
      all = all + push(u,{row=row, d=self:xdist(row,closest)^2}).d end 
    local r = all * math.random()
    local one
    for _,x in pairs(u) do
      one = x
      r   = r - x.d
      if r <= 0 then break end end 
    push(z, one) end
  return z end


