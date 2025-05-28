function neighbors(buckets, d, max)
  local out = {}  -- Initialize output list
  
  -- Recursive helper function to generate neighbors
  local function _go(pos, idx)
    -- If we've processed all dimensions
    if idx > d then
      if not table.equals(pos, buckets) then
        table.insert(out, pos)  -- Add to the output if it's not the original point
      end
      return
    end

    -- For each delta, try -1, 0, and +1 in the current dimension
    for delta = -1, 1 do
      local pos1 = {table.unpack(pos)}  -- Copy the current position
      pos1[idx] = pos1[idx] + delta  -- Modify the current coordinate

      -- Ensure it's within bounds
      if pos1[idx] >= 0 and pos1[idx] < max then
        _go(pos1, idx + 1)  -- Recurse to the next dimension
      end
    end
  end
  
  -- Start the recursion with the initial point and first dimension
  _go(buckets, 1)
  
  return out
end

-- Helper function to compare two tables
function table.equals(t1, t2)
  if #t1 ~= #t2 then return false end
  for i = 1, #t1 do
    if t1[i] ~= t2[i] then return false end
  end
  return true
end



local buckets = {1, 3, 7}  -- A point in 3-dimensional space
local d = #buckets         -- 3 dimensions
local max = 10             -- Maximum value for each coordinate

local neighbors_of_buckets = neighbors(buckets, d, max)

-- Print the neighbors
for i, n in ipairs(neighbors_of_buckets) do
  print("Neighbor " .. i .. ": " .. table.concat(n, ", "))
end

