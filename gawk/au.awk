function List(i)     { split("",i,"") }
function has0(i,k,f) { i[k]["\t"]; @f(i[k]); return k }
function has(i,k,f)  { return has0(i, k?k:length(i[k]), f?f:"List") }
function is(i,x)     { if("is" in i) i.super=x; i.is=x; ++i.id }

function Num(i) { 
  Col(i)
  is(i,"Num")
  has(i,"all","fred")
  

}
BEGIN { def("aa","bb=0,c=fred","d=bb") }
