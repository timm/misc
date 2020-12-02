
local lib = {}

----------------
-- Meta stuff

--- same
function lib.same(x) return x end

-- meta
function lib.map(t,f,      u)
  u,f = {}, type(f)== "function" and f or _ENV[f] 
  for i,v in pairs(t or {}) do u[i] = f(v) end  
  return u
end

----------------
-- Column stuff

--- num
function lib.num () return {lo=math.maxinteger, hi=math.mininteger} end

-----------------
-- String stuff

--- oo
-- Recursive print of a table. Ignore keys starting with "_"
function lib.oo(t,pre,    indent,fmt)
  pre    = pre or ""
  indent = indent or 0
  if indent < 10 then
    for k, v in pairs(t or {}) do
      if not (type(k)=='string' and k:match("^_")) then
        if not (type(v)=='function') then
          fmt = pre..string.rep("|  ",indent)..tostring(k)..": "
          if type(v) == "table" then
            lib.oo(v, pre, indent+1)
          else
            print(fmt .. tostring(v)) end end end end end
end

--- trim
-- Remove leading, trailing, white space
function trim(str) 
  return (str:gsub("^%s*(.-)%s*$", "%1")) end

--- words
function words(str,pat,fun,   t)
  t = {}
  for x in str:gmatch(pat) do t[#t+1] = fun(x) or trim(x) end
  return t
end

--- csv
function lib.csv(file,     ch,fun,   pat,stream,tmp,row)
  stream = file and io.input(file) or io.input()
  tmp    = io.read()
  pat    = "([^".. (ch or ",") .."]+)"
  fun    = tonumber
  return function()
    if tmp then
      row = words(tmp:gsub("[\t\r ]*",""),pat,fun)-- no spaces
      tmp = io.read()
      if #row > 0 then 
        for i,x in pairs(row) do 
          row[i] = tonumber(row[i]) or row[i] end
        return row end
    else
      io.close(stream) 
end end end  

return lib
