-- vim: filetype=lua:ts=2:sw=2:sts=2:et:

local it  ={
  synopois= "Misc lua routines",
  author  = "Tim Menzies, timm@ieee.org",
  license = "MIT",
  year    = 2020 }

local lib={}

-- lib.class
function lib.class(base)
  -- From microlight
  local function update (t,...)
    for i = 1,select('#',...) do
    for k,v in pairs(select(i,...)) do t[k] = v end end
    return t end
  -------------------------------------
  local function import(t,...)
    local other
    t = t or _ENV or getfenv(2)
    local libs = {}
    if select('#',...)==0 then libs[1] = ml
    else for i = 1,select('#',...) do
        local lib = select(i,...)
        if type(lib) == 'string' then
          local value = _G[lib]
          if not value then -- lazy require!
            value = require (lib)
            lib = lib:match '[%w_]+$' end
          lib = {[lib]=value} 
        end
        libs[i] = lib end end
    return update(t,table.unpack(libs)) end
  -------------------------------------
  local klass, base_ctor = {}
  if base then
    import(klass,base)
    klass._base = base
    base_ctor = rawget(base,'_init')
  end
  klass.__index = klass
  klass._class = class
  klass.classof = function(obj)
    local m = getmetatable(obj) -- an object created by class() ?
    if not m or not m._class then return false end
    while m do -- follow the inheritance chain --
      if m == klass then return true end
      m = rawget(m,'_base') 
    end
    return false
  end
  setmetatable(klass,{
      __call = function(klass,...)
        local obj = setmetatable({},klass)
        if rawget(klass,'_init') then
          klass.super = base_ctor
          local res = klass._init(obj,...) 
          if res then obj = setmetatable(res,klass) end
        elseif base_ctor then base_ctor(obj,...) end
        return obj end })
  return klass end

function lib.ok (v1,v2,txt)
  txt = txt and txt or ""
  print(txt.."? ".. (v1 == v2 and "PASS" or "FAIL"))  
end
    
local function egObject() 
  local class=lib.class
  local C = class()
  function C:_init (name) self.name = name end
  function C:__tostring () return 'name '..self.name end
  function C:__eq (other) return self.name == other.name end
  local c = C('Jones')
  assert(tostring(c) == 'name Jones')
  local D = class(C)
  local d = C('Jane') 
  assert(tostring(d) == 'name Jane')
  assert(d == C 'Jane')
  local E = class(D)
  function E:_init (name,nick)
    self:super(name)
    self.nick = nick 
  end
  function E:__tostring () 
    return D.__tostring(self)..' nick '..self.nick 
  end
  lib.ok(tostring(E('Jones','jj')),'name Jones nick jj',"objects")
end

--- Parse command line args.
-- Returns command line as list of lists.
-- Top level are "groups", which start with ":"
-- Second level are either "settings" or "switches"
-- denoted "=x y" (for setting x to y) or "+x","-x"
-- for switching x to true or false (respectively). E.g.
--
-- :bayes =m 2 =bins 5 :kmeans =k 5 -smooth
--
-- returns {bayes= {m=2,bins=5}, kmeans={k=5,smooth=false}}
function lib.args() 
  local i,t,of = 0,{},"--main"
  while i<#arg do
    i=i+1
    local x = arg[i]
    local x1 = x:sub(1,1)
    if     x1==":" then of=x:gsub("^:",""); t[of] = {} 
    elseif x1=="+" then x =x:gsub("^+",""); t[of][x]= true 
    elseif x1=="-" then x =x:gsub("^-",""); t[of][x]= false 
    elseif x1=="=" then x =x:gsub("^=","") 
       i=i+1
       t[of][x] = tonumber(arg[i]) or arg[i]
    else assert(false,"unknown ["..x.."]") end end
  return t
end 

function lib.cli(want, got) 
  got = got or lib.args()
  local out=lib.copy(want)
  for of, t in pairs(got) do
    assert(out[of], "bad of ["..of.."]")
    for x,y in pairs(t) do
      assert(out[of][x], "bad x ["..x.."]")
      assert( type(y) == type(out[of][x]),
              "bad type ["..t1.."] for ["..x.."]")
      out[of][x] = y end end 
  return out
end

-- Deep copy
local function copy(obj,   old,new)
  if type(obj) ~= 'table' then return obj end
  if old and old[obj] then return old[obj] end
  old, new = old or {}, {}
  old[obj] = new
  for k, v in pairs(obj) do new[k] = copy(v, old) end
  return new end

local function egCopy(    x,y)
  x={a=1,b={c=2,d=3},e=4}
  y=copy(x)
  x.b.c=200
  assert(x.b.c ~= y.b.c)
  assert(x.b.d == y.b.d)
end

function lib.oo(t,pre,    indent,fmt)
  pre=pre or ""
  indent = indent or 0
  if indent < 10 then
    for k, v in pairs(t or {}) do
      if not (type(k)=='string' and k:match("^_")) then
        fmt= pre..string.rep("|  ",indent)..tostring(k)..": "
        if type(v) == "table" then
          print(fmt)
          lib.oo(v, pre, indent+1)
        else
          print(fmt .. tostring(v)) end end end end
end

function lib.ooo(z,pre) print(lib.o(z,pre)) end

function lib.o(z,pre,   s,sep) 
  s, sep = (pre or "")..'{', ""
  for _,v in pairs(z or {}) do s = s..sep..tostring(v); sep=", " end
  return s..'}'
end

local function eg()
  egObject()
  lib.oo({a=1,b=2,c={d=1,f=2,e=false}})
  lib.oo(lib.args())
end 

egCopy()

local aa=require "glib/aa"

print(aa(23))

return {tests=eg}

