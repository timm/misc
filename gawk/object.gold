#!/usr/bin/env ./gold
# vim: filetype=awk ts=2 sw=2 sts=2  et :

#--------- --------- --------- -------- -------- --------- ---------
# oo support
func EmptyList(i)     { split("",i,"") }
func Object(i)        { EmptyList(i); i.id = ++AU.id }
func zap(i,k)         { i[k][0]; EmptyList(i[k])} 
func is(i,k)          { isa(i,k,"Object") }
func isa(  i,k,p)     { AU.isa[k]=p; @p(i)    ; i.is=k }
func isas( i,k,p,a)   { AU.isa[k]=p; @p(i,a)  ; i.is=k }
func isass(i,k,p,a,b) { AU.isa[k]=p; @p(i,a,b); i.is=k }

func has( i,k,f)         { f=f?f:"EmptyList"; zap(i,k); @f(i[k]) }
func hass(i,k,f,a)       {                    zap(i,k); @f(i[k],a) }
func hasss(i,k,f,a,b)    {                    zap(i,k); @f(i[k],a,b) }
func hassss(i,k,f,a,b,c) {                    zap(i,k); @f(i[k],a,b,c) }

func how(k,f,   g) {
  while(k) {
    g = k f
    if (g in FUNCTAB) return g
    k = AU.isa[k]
  }
  print "#E> failed method lookup: ["f"]"
  exit 2
}

