#!/usr/bin/env lua
-- vim: paste filetype=lua nospell ts=2 sw=2 sts=2 et :
---------- --------- --------- --------- --------- --

require "lib"

Num={}

function Num.new()
  return Object.new{ako="Num", n=0,mu=0,m2=0,
                    sd=0,lo=10^32, hi= -10^32,w=1}
end

function adds(x,t,f)
 x=x.new()
 f=f or function(z) return z end
 for _,y in pairs(t) do x._isa.add(x, f(y) ) end
 return x
end

function Num.add(i,n)
  i.mu = i.mu + n 
end

n = adds(Num,{10,20,30})
o(n)
