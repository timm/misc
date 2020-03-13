#!/usr/bin/env bash

copyleft() { cat<<'EOF'

######################################################################
# This is free and unencumbered software released into the public domain.

# Anyone is free to copy, modify, publish, use, compile, sell, or
# distribute this software, either in source code form or as a compiled
# binary, for any purpose, commercial or non-commercial, and by any
# means.

# In jurisdictions that recognize copyright laws, the author or authors
# of this software dedicate any and all copyright interest in the
# software to the public domain. We make this dedication for the benefit
# of the public at large and to the detriment of our heirs and
# successors. We intend this dedication to be an overt act of
# relinquishment in perpetuity of all present and future rights to this
# software under copyright law.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
# OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
# ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
# OTHER DEALINGS IN THE SOFTWARE.

# For more information, please refer to <http://unlicense.org/>
######################################################################

EOF
}
lib() { cat <<-EOF
  function has( i,k,f)      { f=f?f:"List"; zap(i,k); @f(i[k]) }
  function hass(i,k,f,m)    {               zap(i,k); @f(i[k],m) }
  function hasss(i,k,f,m,n) {               zap(i,k); @f(i[k],m,n) }
  
  function zap(i,k)         { i[k][0]; List(i[k])} 
  function List(i)          { split("",i,"") }
  function Object(i)        { List(i); i["oid"]=++OID }
EOF
}
fun2awk() { gawk '
  function dots(s) {
    return gensub(/\.([^0-9])([a-zA-Z0-9_]*)/, 
                 "[\"\\1\\2\"]","g",s) 
  } 
  function line(s) {
    if (s ~ /^}/) { 
      show=0
      return s      
    } else {  
      if (s ~ /^[\t ]*$/)              return s                                 
      if (s ~ /^(func|BEGIN|END).*}$/) return dots(s) 
      if (s ~ /^(func|BEGIN|END)/)     show=1  
      return  ( show ?  dots(s) : "# " s ) }
  }
  function main(f,    path, used,        s) {
    if (used[f]++ == 0) { 
      print "#:::::" path " : " f " :::::::::::::::::::::::::::::"
      while((getline s < f) > 0) 
        if (s ~ /^@include/) 
          main( gensub(/.*"(.*)".*$/, "\\1","g",s), path ": " f, used)
        else if (f ~ /.*awk$/)
          print s
        else 
          if ((s !~ /^#!/)  && (s !~ /^# vim/))
            print line(s) }
  }
  BEGIN { main("'$1'")}'
}

Bin=$HOME/opt/fun/bin
mkdir -p $Bin 

File=${1##*/}  
Stem=${File%.*}
Out=$Bin/$Stem

(echo "#!/usr/bin/env gawk -f"
 copyleft
 lib 
 fun2awk $1
 Main=${Stem}Main
 echo 'BEGIN { if("'${Main}'" in FUNCTAB) '${Main}'()}'
) > $Out

chmod +x $Out

shift
if    [ ! -t 0 ]
then  cat - |  $Out $*
else  $Out $*
fi
