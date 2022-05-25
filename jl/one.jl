using Parameters

@with_kw  mutable struct A          
       a=5                   
       b="hello"          
       c="23"                 
       end;    
			 
x=A(c=90)

print(x.c)