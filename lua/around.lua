-- Function to get the neighbors of a point in d-dimensional space
function neighbors(buckets, d, max)
  local neighbors_list = {}
  
  -- Generate all possible neighbors for each dimension
  -- This function computes the 3^d neighbors by moving in each dimension by -1, 0, or 1
  local function get_neighbors(coords, idx)
    if idx > d then
      -- We've generated a full coordinate, add it to the list (excluding the original point)
      if not table.equals(coords, buckets) then
        table.insert(neighbors_list, coords)
      end
      return
    end
    
    -- For each dimension, try -1, 0, and 1 (relative to the current coordinate)
    for delta = -1, 1 do -- Move by -1, 0, and +1 (relative to the current coordinate)
      -- Create a copy of the coordinates
      local new_coords = {table.unpack(coords)}
      new_coords[idx] = new_coords[idx] + delta
      
      -- Ensure the new coordinates stay within bounds
      if new_coords[idx] >= 0 and new_coords[idx] < max then
        -- Recurse for the next dimension
        get_neighbors(new_coords, idx + 1)
      end
    end
  end
  
  -- Start the recursion with the given bucket position
  get_neighbors(buckets, 1)

  return neighbors_list
end

-- Helper function to compare two tables
function table.equals(t1, t2)
  if #t1 ~= #t2 then return false end
  for i = 1, #t1 do
    if t1[i] ~= t2[i] then return false end
  end
  return true
end

-- Example usage
local buckets = {3,3,3}  -- A point in 3-dimensional space
local d = #buckets         -- 3 dimensions
local max = 10             -- Maximum value for each coordinate

local neighbors_of_buckets = neighbors(buckets, d, max)

-- Print the neighbors
for i, n in ipairs(neighbors_of_buckets) do
  print("Neighbor " .. i .. ": " .. table.concat(n, ", "))
end

