BEGIN { Big=1E32; 
        nN="n"; poS="pos"; txT="txt"; 
        mU="mu"; m2="m2"; sD="sd"; hI="hi"; lO="lo"; 
        xX="x"; yY="y"; alL="all"; nameS="names"
        klasS="klass"
        alL="all"; mosT="most"; modE="mode" ; goaL="goal"}

BEGIN{ FS=1; Big=1E32 }
     { NR==1 ? header() : data() }
END  { main(); rogues() }

function NUM(i,txt,pos) {
  i[txT]  = txt
  i[poS]  = pos
  i[nN]   = i[mU] = i[m2] = i[sD] = 0
  i[hI]   = -(i[lO] = Big)
  i[goaL] = }

function SYM(i,txt,pos) {
  i[txT] = txt
  i[poS] = pos
  have(i,alL)
  i[nN]   = i[mosT] = 0
  i[modE]= ""  }

function COLS(i,a) {
  have(i,nameS)
  have(i,alL)
  have(i,xX)
  have(i,yY)
  for(pos in a) { 
    name = a[pos]
    i[nameS][pos] = a[pos] 
    hAVE(i,alL, (name ~ /^[A-Z]/) ? "NUM" : "SYM", a[pos], pos) 
    if (name ~ /X$/) {
      i[/[!+-]$/ ? yY : xX] = pos
      if (name ~ /!$/) have(i,klasS) = pos }}}

function have(a,k)         { k=="" ? length(a)+1 : k; a[k][0]=0; delete a[k][0]; return k}
function havE(a,k,F)       { k = have(a,k); @F(k);       return k }
function haVE(a,k,F,x)     { k = have(a,k); @F(k,x);     return k }
function hAVE(a,k,F,x,y)   { k = have(a,k); @F(k,x,y);   return k }
function HAVE(a,k,F,x,y,z) { k = have(a,k); @F(k,x,y,z); return k }

function header(     i) {
  srand(SEED ? SEED : 1234567891)
  for(i=1;i<=NF;i++) {
    if ($i ~ /^[A-Z]/) Num[i] = $i ~ /\+$/
    if ($i ~ /[!+-]$/) Y[i] else X[i]
  }
  for(i in Num) Hi[i] = - (Lo[i]=Big) }

function data(    i) {
  for(i=1;i<=NF;i++)
    if (i != "?") {
      if (i in Num)  {
        $i += 0
        Lo[x] = min($i, Lo[x])
        Hi[x] = max($i, Hi[x]) }
  }
  Row[NR-1][i]=$i }

