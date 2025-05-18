local lines = {}
for line in io.lines() do table.insert(lines, line) end

local i = 1
while i <= #lines do
  local line = lines[i]
  local nextline = lines[i + 1] or ""

  local defname = line:match("^%s*def%s+([%w_]+)")
  local classname = line:match("^%s*class%s+([%w_]+)")
  local doc = nextline:match('^%s*"(.-)"')

  if classname and doc then
    print("## " .. classname)
    print("\n# " .. doc)    -- 🔧 prefix with `#` to force markdown rendering
    print(line)
    i = i + 2
  elseif defname and doc then
    local indent = line:match("^(%s*)") or ""
    print(indent .. "# " .. doc)
    print(line)
    i = i + 2
  else
    print(line)
    i = i + 1
  end
end

