<a name=top></a>
<p align=center>
<a href="https://github.com/timm/lump/blob/master/READ.md#top">home</a> ::
<a href="https://github.com/timm/lump/blob/master/READE.md#contribute">contribute</a> ::
<a href="https://github.com/timm/lump/issues">issues</a> ::
<a href="https://github.com/timm/lump/blob/master/LICENSE.md">&copy;2020<a> by <a href="http://menzies.us">Tim nzies</a>
</p>

<h1 align=center> LU v0.1<br>(print (list (of (some :LISP "tricks")))) </h1>

<p align=center>
<img src="https://imgs.xkcd.com/comics/lisp_cycles.png"><br>
<img src="https://img.shields.io/badge/purpose-ai%20,%20se-blueviolet"> <a 
href="https://github.com/timm/lump/blob/master/LICENSE.md"> <img  
   alt="License" src="https://img.shields.io/badge/license-mit-red"></a> <a 
  href="https://zenodo.org/badge/latestdoi/289524083"> <img 
  src="https://zenodo.org/badge/289524083.svg" alt="DOI"></a> <img 
alt="Platform" src="https://img.shields.io/badge/platform-osx%20,%20linux-lightgrey"> <img 
alt="lisp" src="https://img.shields.io/badge/language-sbcl,clisp-blue"> <a 
 href="https://travis-ci.org/github/timm/lump"><img alt="tests" 
   src="https://travis-ci.org/timm/lump.svg?branch=master"></a>
</p> 

## About the code

- [About the code](#about-the-code) 
- [Config](#config) 
- [Lib](#lib) 
    - [Maths](#maths) 
    - [Strings](#strings) 
    - [Printing a Table](#printing-a-table) 
    - [Meta](#meta) 
        - [same(z) : return z](#samez--return-z) 
        - [map(t,f) : apply `f` to everything in `t` and return the result](#maptf--apply-f-to-everything-in-t-and-return-the-result) 
        - [copy(t) : return a deep copy of `t`](#copyt--return-a-deep-copy-of-t) 
    - [Lists](#lists) 
        - [any(a) : sample 1 item from `a`](#anya--sample-1-item-from-a) 
        - [anys(a,n) : sample `n` items from `a`](#anysan--sample-n-items-from-a) 
        - [keys(t): iterate over key,values (sorted by key)](#keyst-iterate-over-keyvalues-sorted-by-key) 


This is a _one file_ system where all the code
is in a markdown file and extraced using

    sh ell --code

## Config
```lua
My = {aka={},
      id=0
}

function id (x)
	if not x._id then my.id=my.id+1; x._id= my.id end
	return x._id 
end

function eg_id(x, a) a={}; id(a); print(a._id) end

function c(s,k)   return string.sub(s,1,1)==k end
function klass(x) return c(x,"!")  end 
function less(x)  return c(x,"<")  end
function goal(x)  return c(x,">")  or less(x) end
function num(x)   return c(x,"$")  or goal(x) end
function y(x)     return klass(x)  or goal(x) end
function x(x)     return not y(x)   end
function sym(x)   return not num(x) end
function xsym(z)  return x(z) and  sym(z) end
function xnum(z)  return x(z) and  num(z) end

function cols(all,f)
  return select(all, function(z) return f(z.txt) end)
end
```

## Lib
### Maths
```lua
function round(num, places)
  local mult = 10^(places or 0)
  return math.floor(num * mult + 0.5) / mult
end
```
### Strings

Support string interpolation with "_%_":
- e.g. `print( "%5.2f" % math.pi )`
- e.g. `print( "%-10.10s %04d" % { "test", 123 } )`

```lua
getmetatable("").__mod = function(a, b)
  local f, u = string.format, table.unpack
  return (b and type(b)=="table" and f(a, u(b)) or f(a,b)) or a
end

```
### Printing a Table

- For flat tables
  - `o` generates, but does not print, a print string.
  - `oo` generates a prints a string.
- For nested tables
  - `ooo` generates and prints a nested string

```lua
function o(z,pre,   s,sep) 
  s, sep = (pre or "")..'{', ""
  for _,v in pairs(z or {}) do s = s..sep..tostring(v); sep=", " end
  return s..'}'
end

function oo(z,pre) print(o(z,pre)) end

function ooo(t,pre,    indent,fmt)
  pre=pre or ""
  indent = indent or 0
  if indent < 10 then
    for k, v in pairs(t or {}) do
      if not (type(k)=='string' and k:match("^_")) then
        fmt= pre..string.rep("|  ",indent)..tostring(k)..": "
        if type(v) == "table" then
          print(fmt)
          ooo(v, pre, indent+1)
        else
  print(fmt .. tostring(v)) end end end end
end
```

### Meta
#### same(z) : return z
```lua
function same(z) return z end
```
#### map(t,f) : apply `f` to everything in `t` and return the result
```lua
function map(t,f, u)
  u, f = {}, f or same
  for i,v in pairs(t or {}) do u[i] = f(v) end  
  return u
end
```
#### copy(t) : return a deep copy of `t`
```lua
function copy(t)  
  return type(t) ~= 'table' and t or map(t,copy)
end

function select(t,f,     g,u)
  u, f = {}, f or same
  for _,v in pairs(t) do if f(v) then u[#u+1] = v  end end
  return u
end


### Lists
#### any(a) : sample 1 item from `a`
```lua
function any(a) return a[1 + math.floor(#a*math.random())] end
```
#### anys(a,n) : sample `n` items from `a`
```lua
function anys(a,n,   t) 
  t={}
  for i=1,n do t[#t+1] = any(a) end
  return t
end
```
#### keys(t): iterate over key,values (sorted by key)
```lua
function keys(t)
  local i,u = 0,{}
  for k,_ in pairs(t) do u[#u+1] = k end
  table.sort(u)
  return function () 
    if i < #u then 
      i = i+1
      return u[i], t[u[i]] end end 
end

function csv(file,     stream,tmp,row)
  function s2t(s,     sep,t)
    t, sep = {}, sep or ","
    for y in string.gmatch(s,"([^"..sep.."]+)") do 
       t[#t+1] = tonumber(y) or y 
    end
    return t
  end
  stream = file and io.input(file) or io.input()
  tmp    = io.read()
  return function()
    if tmp then
      tmp= tmp:gsub("[\t\r ]*","") -- no whitespace
      row= s2t(tmp)
      tmp= io.read()
      if #row > 0 then return row end
    else
  io.close(stream) end end   
end
for row in csv("data/weather4.csv") do
  oo(row)
end

function within(x,y,z)
  assert(x <= y and y <= z, 'outside range ['..x..' to '..']')
end

function rogues(   no)
  no = {the=true, TESTING=true,
              jit=true, utf8=true, math=true, package=true,
              table=true, coroutine=true, bit=true, os=true,
              io=true, bit32=true, string=true, arg=true,
              debug=true, _VERSION=true, _G=true }
  for k,v in pairs( _G ) do
    if type(v) ~= "function" and not no[k] then
      if k:match("^[^A-Z]") then
        print("-- ROGUE ["..k.."]") end end end
end

function nok(t) return true end

do 
  local  yes,no = 0,0 
  function egs(t,      t1,t2, passed,err)
    for s,x in pairs(_G) do  
      if s:match("^eg_") then
        if t and not s:match("^eg_"..t) then break end
        yes = yes + 1
        t1 = os.clock()
        math.randomseed(1)
        passed,err = pcall(x) 
        if passed then
           t2= os.clock()
           print(string.format(s.." PASS! %8.6f secs", t2-t1))
        else
          no = no + 1
          print(string.format(s.." FAILED! : %s [%.0f] %%",
                              err:gsub("^.*: ",""), 
                              100*yes/(yes+no))) end 
    end end
  end
  rogues()
  return yes - no == 1
end

function eg_test()
  assert(1==2)
end

function eg_test2()
  assert(1==1)
end

if arg[1] == "-T" then 
  local status = tests(arg[2])
  rogues() 
  os.exit(status)
end
```
