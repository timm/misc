<a name=top></a>
<p align=center>
<a href="https://github.com/timm/lump/blob/master/READ.md#top">home</a> ::
<a href="https://github.com/timm/lump/blob/master/READE.md#contribute">contribute</a> ::
<a href="https://github.com/timm/lump/issues">issues</a> ::
<a href="https://github.com/timm/lump/blob/master/LICENSE.md">&copy;2020<a> by <a href="http://menzies.us">Tim Menzies</a>
</p>

<h1 align=center> LUMP v0.1<br>cluster and contrast</h1>

<p align=center>
<img src="https://i0.wp.com/studentwork.prattsi.org/infovis/wp-content/uploads/sites/3/2019/04/image-44.png?w=758&ssl=1"><br>
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


- [Config](#config) : 
    - [My](#my--global-with-all-settings) : global with all settings
- [Data](#data) : 
    - [Columns](#columns) : 
        - [Define column types](#define-column-types) : 
- [Lib](#lib) : 
    - [Maths](#maths) : 
        - [round(n,places)](#roundnplaces--round-n-to-some-decimal-places) : round `n` to some decimal `places`.
    - [Strings](#strings) : 
        - [Interpolation](#interpolation) : 
        - [o(t,pre)](#otpre--return-t-as-a-string-with-prefix) : return `t` as a string, with `pre`fix
        - [oo(t,pre)](#ootpre--print-t-as-a-string-with-prefix) : print `t` as a string, with `pre`fix
        - [ooo(t,pre)](#oootpre--return-a-string-representing-ts-recursive-contents) : return a string representing `t`'s recursive contents.
    - [Meta](#meta) : 
        - [id(x)](#idx--ensure-x-has-a-unique-if) : ensure `x` has a unique if
        - [same(z)](#samez--return-z) : return z
        - [map(t,f)](#maptf--apply-f-to-everything-in-t-and-return-the-result) : apply `f` to everything in `t` and return the result
        - [copy(t)](#copyt--return-a-deep-copy-of-t) : return a deep copy of `t`
        - [select(t,f)](#selecttf--return-a-table-of-items-in-t-that-satisfy-function-f) : return a table of items in `t` that satisfy function `f`
    - [Lists](#lists) : 
        - [any(a)](#anya--sample-1-item-from-a) : sample 1 item from `a`
        - [anys(a,n)](#anysan--sample-n-items-from-a) : sample `n` items from `a`
        - [keys(t)](#keyst-iterate-over-keyvalues-sorted-by-key) : iterate over key,values (sorted by key)
    - [Files](#files) : 
        - [csv(file)](#csvfile--iterate-through--non-empty-rows-divided-on-comma-coercing-numbers) : iterate through  non-empty rows, divided on comma, coercing numbers
- [Testing](#testing) : 
    - [Support code](#support-code) : 
        - [within(x,y,z)](#withinxyz-y-is-between-x-and-z) : `y` is between `x` and `z`.
        - [rogues()](#rogues-report-escaped-local-variables) : report escaped local variables
        - [egs(x)](#egsx-run-the-test-function-egx-or-if-x-is-nil-run-all) : run the test function `eg_x` or, if `x` is nil, run all.
    - [Unit tests](#unit-tests) : 
- [Main](#main) : 



This is a _one file_ system where all the code
is in a markdown file and extraced using

    sh ell --code

## Config
### My : global with all settings
```lua
My = {aka={},
      id=0,
      seed=1,
      test= {yes=0,no=0}
}
```
## Data
### Columns
#### Define column types
```lua
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
#### round(n,places) : round `n` to some decimal `places`.
```lua
function round(num, places)
  local mult = 10^(places or 0)
  return math.floor(num * mult + 0.5) / mult
end

```
### Strings
#### Interpolation
Support string interpolation with "_%_":
- e.g. `print( "%5.2f" % math.pi )`
- e.g. `print( "%-10.10s %04d" % { "test", 123 } )`

```lua
getmetatable("").__mod = function(a, b)
  local f, u = string.format, table.unpack
  return (b and type(b)=="table" and f(a, u(b)) or f(a,b)) or a
end
```
#### o(t,pre) : return `t` as a string, with `pre`fix
```lua
function o(z,pre,   s,sep) 
  s, sep = (pre or "")..'{', ""
  for _,v in pairs(z or {}) do s = s..sep..tostring(v); sep=", " end
  return s..'}'
end
```
#### oo(t,pre) : print `t` as a string, with `pre`fix
```lua
function oo(z,pre) print(o(z,pre)) end
```
#### ooo(t,pre) : return a string representing `t`'s recursive contents.
```lua
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
#### id(x) : ensure `x` has a unique if
```lua
function id (x)
	if not x._id then My.id=My.id+1; x._id= My.id end
	return x._id 
end
```
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
```
#### select(t,f) : return a table of items in `t` that satisfy function `f`
```lua
function select(t,f,     g,u)
  u, f = {}, f or same
  for _,v in pairs(t) do if f(v) then u[#u+1] = v  end end
  return u
end
```
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
```
### Files
#### csv(file) : iterate through  non-empty rows, divided on comma, coercing numbers
```lua
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
```
## Testing
### Support code
#### within(x,y,z): `y` is between `x` and `z`.
```lua
function within(x,y,z)
  assert(x <= y and y <= z, 'outside range ['..x..' to '..']')
end
```
#### rogues(): report escaped local variables
```lua
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
```
#### egs(x): run the test function `eg_x` or, if `x` is nil, run all.
```lua
function egs(t,      t1,t2, passed,err,y,n)
  for s,x in keys(_G) do  
    if s:match("^eg_") then
      if t and not s:match("^eg_"..t) then break end
      My.test.yes = My.test.yes + 1
      t1 = os.clock()
      math.randomseed(My.seed)
      passed,err = pcall(x) 
      if passed then
         t2= os.clock()
         print(string.format("PASS! "..s.." \t: %8.6f secs", t2-t1))
      else
        My.test.no = My.test.no + 1
        y,n = My.test.yes,  My.test.no
        print(string.format("FAIL! "..s.." \t: %s [%.0f] %%",
                            err:gsub("^.*: ",""), 
                            100*y/(y+n))) end 
end end end
```
### Unit tests
```lua 
function eg_test()   assert(1==2) end
function eg_rnf()    assert(3.2==round(3.2222,1)) end
function eg_intrp()  assert("3.14" == (("%4.2f" % math.pi))) end
function eg_o()      assert("{1, aa, 3}" == o({1,"aa",3})) end
function eg_id(  a)  a={}; id(a); id(a); assert(1==a._id) end
function eg_map( t)
	assert(30 == map({1,2,3}, function (z) return z*10 end)[3])
end
```
## Main
```lua
if arg[1] == "-T" then 
  local status = egs(arg[2])
  rogues() 
  os.exit((My.test.yes - My.test.no == 1) and 0 or 1)
end
```
