make() = include("one.jl")

using Parameters

@with_kw mutable struct A          
       a=5                   
       b="hello"          
       c="23"                 
       end;    
			 

x=A(c=911)

test(y=2) = print(x.c,y)
