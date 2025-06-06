#!/usr/bin/env gawk -f
BEGIN{FS=OFS=",";srand(1234567891);p=2;Few=32;Stop=4}
NR==1{ncols=NF;for(i=1;i<=NF;i++)name[i]=$i;x=0;for(i=1;i<=NF;i++)xcol[i]=($i!~"[+-]$");next}
{x++;for(i=1;i<=NF;i++){a[x,i]=$i+0;if(xcol[i]){lo[i]=(x==1)?a[x,i]:(a[x,i]<lo[i]?a[x,i]:lo[i])
hi[i]=(x==1)?a[x,i]:(a[x,i]>hi[i]?a[x,i]:hi[i])}}}
function norm(i,x){return(x-lo[i])/((hi[i]-lo[i])+1e-32)}
function xdist(i,j, d,n,k){for(k=1;k<=ncols;k++)if(xcol[k]){d+=((norm(k,a[i,k])-norm(k,a[j,k]))^p);n++}
return(d/n)^(1/p)}
function kpp(k,    i,s,c,d,r,j,min,r1,r2){c[1]=int(1+rand()*x);slen=(Few<x)?Few:x
for(i=1;i<=slen;i++)s[i]=int(1+rand()*x)
for(i=2;i<=k;i++){for(j=1;j<=slen;j++){min=""
for(z=1;z<i;z++){r1=s[j];r2=c[z];d=xdist(r1,r2)^2;if(min==""||d<min)min=d}dist[j]=min}
r=rand()*sum(dist,slen);for(j=1;j<=slen;j++){r-=dist[j];if(r<=0){c[i]=s[j];break}}}}
function sum(v,n, i,s){for(i=1;i<=n;i++)s+=v[i];return s}
END{kpp(Stop)}
