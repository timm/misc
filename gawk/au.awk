#!/usr/bin/env ./au
function has(i,f,k)  { return has0(i, f?f:"List", k?k:1+length(i[k])) }
function has0(i,f,k) { i[k][0]; @f(i[k]); delete i[k][0]; return k }
function List(i)     { split("",i,"") }
function is(i,x)     { if("is" in i) i["super"]=x; i["is"]=x; ++i["id"] }

function Num(i) { 
  Col(i)
  is(i,"Num")
  has(i,"fred","all")
}
BEGIN {print 1}
