# vim: nospell filetype=awk ts=2 sw=2 sts=2  et :
#--------- --------- --------- --------- --------- ---------

@include "gold"

# meta["au"] 

function either(x,y) { 
  return x=="" ? y : x }

function isnum(x) {
  return x=="" ? 0 : x == (0+strtonum(x)) }
