math.randomseed(1234567891)

function trim(s) return s:match"^%s*(.-)%s*$" end

function coerce(s) return tonumber(s) or trim(s) end

function cells(s,     t)
  t={}; for s1 in s:gmatch"([^,]+)" do t[1+#t] = coerce(s) end; return t end

function csv(   t,names, row)
  t = {}; for line in io.lines() do 
            if names then t[1+#t] = cells(line) else name= cells(line) end end
  return names,t end

-- Normalize a column
function mids(names,data, col)
  if not names[col]:find"^[A-Z]" then return end
  sum,n,{}
  for r, row in pairs(data) do
    if r>1 and type(row[col]) == "number" then
      lo = math.min(lo, row[col])
      hi = math.max(hi, row[col]) end end
  for r, row in pairs(data) do
    if r>1 and type(row[col]) == "number" then
      row[col] = (row[col] - min) / (max - min + 1E-32) end end end

function normalize(names,data, col)
  if not names[col]:find"^[A-Z]" then return end
  lo,hi = math.huge,-math.huge
  for r, row in pairs(data) do
    if r>1 and type(row[col]) == "number" then
      lo = math.min(lo, row[col])
      hi = math.max(hi, row[col]) end end
  for r, row in pairs(data) do
    if r>1 and type(row[col]) == "number" then
      row[col] = (row[col] - min) / (max - min + 1E-32) end end end

function least(t,FUN)
 lo=math.huge
 for _,x in pairs(t) do tmp=FUN(x); if tmp<lo then lo,out=tmp,x end end
 return x end

function dist(names,t1,t2)
  for i,txt in pairs(names) do
   if not txt:find"[!+-]$" then: n=n+1; d=d+math.abs(t1[i] - t2[i])^p end end
  return maths.sqrt(d/#names) end

function kmeans(names, data, k, max_iter, clusters)
  centroids = {}
  for i = 1, k do
    push(centroids, {rows={}, error=0,center= data[math.random(#data)]) end
  for _ = 1, max_iter do
    for _, row in pairs(data) do
      where = least(centroids, function(c) return dist(names,c.center,row) end)
      where.error = where.error + dist(row,where.center)
      push(where.rows, row) end 
      

    for i, cluster in ipairs(clusters) do
      local new_centroid = {}
      for j = 1, #cluster[1] do
        local sum = 0
        for _, row in ipairs(cluster) do sum = sum + row[j] end
        new_centroid[j] = sum / #cluster
      end
      centroids[i] = new_centroid
    end
  end

  return centroids
end

-- Example usage
local data = parse_csv()
normalize(data, 1) -- Normalize column 1
local centroids = kmeans(data, 3, 10)
for _, centroid in ipairs(centroids) do
  print(table.concat(centroid, ", "))
end

