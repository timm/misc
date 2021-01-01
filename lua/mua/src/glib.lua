-- vim: filetype=lua:ts=2:sw=2:sts=2:et:

local my  ={
  synopois= "Misc lua routines",
  author  = "Tim Menzies, timm@ieee.org",
  license = "MIT",
  year    = 2020 }

local lib={}

local function update (t,...)
  for i = 1,select('#',...) do
  for k,v in pairs(select(i,...)) do t[k] = v end end
  return t end

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

function lib.class(base)
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

function ok (v1,v2,txt)
  txt = txt and txt or ""
  print(txt.."? ".. (v1 == v2 and "PASS" or "FAIL"))  
end
    
local function objecttests() 
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
  ok(tostring(E('Jones','jj')),'name Jones nick jj',"objects")
end

local function tests() objecttests() ; end

return {tests=tests}

