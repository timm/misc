local i, a = 1, {}
for now in io.lines() do a[1 + #a] = now end
while i <= #a do
  local now, after = a[i], a[i + 1] or ""
  local isDef      = now:match("^%s*def%s+([%w_]+)")
  local isClass    = now:match("^%s*class%s+([%w_]+)")
  local doc        = after:match('^%s*"(.-)"')

  if isClass and doc then
    print("# ### " .. isClass .. "\n\n# " .. doc .. "\n" .. now)    
    i = i + 2
  elseif isDef and doc then
    local indent = now:match("^(%s*)") or ""
    print(indent .. "# " .. doc .. "\n" .. now)
    i = i + 2
  else
    print(now)
    i = i + 1 end end

