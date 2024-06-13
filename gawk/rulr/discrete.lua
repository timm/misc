local RANGE={}
function RANGE.new(pos,name,lo,hi,n)
  return l.is(RANGE,{pos=pos, name=name, lo=lo, hi=hi or lo, n=n or 0, ys={}}) end

function RANGE:add(x,y)
  self.n  = self.n + 1
  self.lo = math.min(x, self.lo)
  self.hi = math.max(x, self.hi)
  self.ys[y] = 1 + (self.ys[y] or 0) end

function RANGE:mergeable(other, small)
  both      = RANGE.new(self.pos, self.name, self.lo, other.hi, self.n + other.n)
  both.ys   = l.sumDict(self.y, other.ys)
  e1,e2,e12 = l.entropy(self.ys), l.entropy(other.ys), l.entropy(both.ys)
  if self.n < small or other.n < small or e12 <= (self.n*e1 + other.n*e2) / both.n then
    return both end end 

function DATA:bins(col,klasses)
  bins,n = {},0
  for klass,rows in pairs(klasses) do
    for _,row in pairs(rows) do
      n = n + 1
      x = row[col.pos]
      if x ~= "?" then 
        k = col:bins(x) 
        bins[k] = bins[k] or RANGE.new(col.pos, col.name, x)
        bins[k]:add(x,klass) end end end 
  return col:merges(sort(bins, by"lo"),  n/the.bins) end

function SYM:bin(x) return x end
function NUM:bin(x) return math.min(the.bins, 1+((the.bins * self:norm(x))//1)) end
 
function SYM:merges(x,_) return x end
function NUM:merges(b4,enough,    j,now,a,tmp)
  j, now = 1, {}
  while j <=  len(b4) do
    a = b4[j]
    if j <  len(b4) then
      tmp = a:mergable(b4[j+1],enough) 
      if tmp then
        a = tmp
        j = j+1 end end
    l.push(now,a)
    j = j+1 
  end
  if len(now) < len(b4) then return self:merges(now,enough) end
  for j=2,len(now) do
    now[j].lo    = now[j-1].hi
    now[1].lo    = -1E30
    now[#now].hi =  1E30 end
  return now end