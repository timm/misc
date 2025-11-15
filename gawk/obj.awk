BEGIN { ID=1, BIG=1E30 }

function has(a,i)         { a[i][0]; del a[i][0]; return i }
function more(a,i,     j) { j=length(a)+1; has(a,j); has(a[j],i); return j }
function isa(i,x)         { split("",i,""); i[Is]=x; i[Id] = ++ID }

function obj(x,names,     a,i)  { 
  split("Id,Is," names,a,","); for(i in a) {OF[x][i] = a[i]; SYMTAB[a[i]] = i}}

function lint(      x,y,i,j) {
  for (x in OF) for (y in OF)
      if (y > x)
        for(i in OF[x]) for(j in OF[y])
            if ((i != j) && OF[x][i] == OF[y][j])
              print("E",OF[x][i]," and ",OF[y][j]," have bad indexes ",i,j) }

function add(i,x,              f) { f=i[Is]"add";  return &f(i,x) }
function dist(i,x,y,           f) { f=i[Is]"dist"; return &f(i,x,y) }
function like(i,x,y,prior,     f) { f=i[Is]"dist"; return &f(i,x,y,prior) }

BEGIN { obj("Sym","N,At,Txt,Seen,Most,Mode")    }
BEGIN { obj("Num","N,At,Txt,Heaven,M,M2,Lo,Hi") }

function SYM(i,at,txt) { 
  _col(i,at,txt,"SYM"); has[i,Seen]; i[Most]=0; i[Mode]="" }

function NUM(i,at,txt) { 
  _col(i,at,txt,"NUM"); i[Mu]=i[M2]=0; i[Lo]=BIG; i[Hi]= -BIG; i[Heaven]= txt !~ /"-$"/ }

function _col(i,at txt,is) {
  isa(i,is); i[N]=0 i[At]=at ? at : 0 i[Txt]=txt ? txt : "" }
