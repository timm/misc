#!/usr/bin/env lua
-- vim: paste filetype=lua nospell ts=2 sw=2 sts=2 et :
---------- --------- --------- --------- --------- --

require "lib"

Num={}

function Num.new()
  return Object.new{ako="Num", n=0,mu=0,m2=0,
                    sd=0,lo=10^32, hi= -10^32,w=1}
end

function adds(i,t,f)
 i = i.new()
 for _,y in pairs(t) do add1(i,y,f) end
 return i
end

function add1(i,n,f)
  if n=="?" then return n end
  return i._isa.add(i, f and f(n) or n)
end

function Num.add(i,n)
  i.mu = i.mu + n 
end

n = adds(Num,{10,20,30})
o(n)
