#!/usr/bin/env gawk -f
BEGIN {FS=","}
      {gsub(/[ \t]/,"")}
NR==1 {for(i=1;i<=NF;i++) 
         if ($i !~ /X$/) {
           name[i] = $i
           if ($i ~ /^[A-Z]/) {hi[i]= -(lo[i]=1e32); n[i]=m2[i]=mu[i]=sd[i]=0}
           if ($i ~ /-$/) y[i]=-1
           if ($i ~ /+$/) y[i]= 1 }}
NR>1  {for(i in lo) 
         if (i != "?") {
           $i += 0 
           d = mu[i] - $i

function NUM(s) {
  tmp= {txt=s, lo=1E32, hi=1E32, mu=0, m2=0, sd=0, goalp=nil}
  if s:find"-$" then tmp.goalp=  1 end
  if s:find"+$" then tmp.goalp= -1 end
  return tmp end

  
      
function new(klass,obj)
  klass.__index=klass; return setmetatable(obj,klass) end


