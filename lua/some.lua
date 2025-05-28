Some = {most = 256}

function Some.new()
  return setmetatable({n = 0, has = {}, sorted = false}, {__index = Some})
end

function Some:add(x, at)
  self.n = self.n + 1
  if #self._has < Some.most then at = #self._has + 1
  elseif R() < Some.most / self.n then at = math.random(#self._has) end
  if at then self._has[at] = x; self.sorted = false end end 

function Some:lo()    return tile(0.0, self:has()) end
function Some:hi()    return tile(1.0, self:has()) end
function Some:med()   return tile(0.5, self:has()) end
function Some:sd(  h) h=self:has(); return (tile(0.9,h) - tile(0.1,h))/2.56 end

function Some:has()
  if not self.sorted then table.sort(self._has); self.sorted=true end
  return self._has end

function tile(p, t) return t[math.floor(#t * p + 0.5)] end

