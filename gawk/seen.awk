BEGIN{FS=","}
NR==1{for(i=1;i<=NF;i++) Names[i]=$i

function add(i,x,    f) { f="add"i["is"];  return @f(i,x) }
function mid(i,x,    f) { f="mid"i["is"];  return @f(i,x) }
function div(i,x,    f) { f="div"i["is"];  return @f(i,x) }
function like(i,x,p, f) { f="like"i["is"]; return @f(i,x,p) }

function List(i) { split("",i,"") }

function Col(i,name,at) {
  List(i)
  i["name"]= name ? name : " "
  i["at"] = at ? at  : 0 
  i["n"]  = 0
  i["heaven"] = name ~ /-$/ ? 0 : 1 }

function Num(i,name,at) {
  Col(i,name,at)
  i["is"] = "Num"
  i["lo"] =  1E30
  i["hi"] = -1E30
  i["mu"] = i["sd"] = i["m2"] = 0 }

function Sym(i,name,at) {
  Col(i,name,at)
  i["is"] = "Sym"
  has(i,"has") }

function Data(i,names) {
  List(i)
  has(i,"rows")
  has(i,"cols","Cols",names) }

function Cols(i,names,    klass) {
  List(i)
  has(i,"all")
  has(i,"x")
  has(i,"y")
  for(j in names) {
    i["names"][j] = names[j]
    klass = name  ~ /^[A-Z]/ ? "Num" : "Sym"
    has(i["all"], j, klass, names[j], j) }}

function has(i,k)          { i[k][1]; delete i[k][1] }
function has(i,k,f)        { has(i,k);  @if (f(i[k]) }     ;return k}
function has2(i,k,f,x)     { has(i,k);  @f(i[k],x) }    ;return k}
function has3(i,k,f,x,y)   { has(i,k);  @f(i[k],x,y) }
