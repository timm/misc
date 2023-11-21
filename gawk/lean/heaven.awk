function make(s,    fun)
  function fun(s) return s=="true" or (s~="false" and s) end
  return math.tointeger(s) or tonumber(s) or fun(s:match'^%s*(.*%S)') end

function csv(src) 
  src = io.input(src)
  return function(    s,t)
    s,t = io.read(),{}
    if   s 
    then for s1 in s:gmatch("([^,]+)") do t[1+#t]=make(s1) end; return t
    else io.close(src) end end end 

function main(file,     rows,cols)
  for t in csv(file) do    
    (cols and head or body)() rows[1+#rows] = add(cols,) 
    then for _,col in pairs(cols) do add(col,t[col.at]) end
    else 
     

      BEGIN {FS=","; Big=1E30; Best=.1}
      {NR==1 ? head() ; body()}
END   {main()}

function NUM(i,txt) {
  i["txt"] = txt
  i["w"]  =  (txt ~ /[-]$/) ? -1 : 1
  i["lo"] =  big
  i["hi"] = -big }

function add(col,x) {
  x += 0
  if (x < num["lo"]) num["lo"] = x
  if (x > num["hi"]) num["hi"] = x}
  return x }

function main(      i,n,m) {
  for(i in Row) 
     Row[i]["d2h"] = d2hs(Row[i]["cells"])
   n=asort(Row,Row1,"sortByD2h")}
   for(i in Row1)
    
function head(      i) {
  for(i=1;i<=NF;i++)  
    if ($i ~ /^[A-Z]) has(NUMS, i, "NUM", $i);

function body(           i) {
  for(i=1;i<=NF;i++) 
    Row[NF-1]["cells"][i] = (i in NUMS) ? add(NUMS[i], $i) : $i

function d2h(num,x)  { return abs( a[[i] - norm(x,num["lo"],num["hi"]))  }

function d2hs(a,    n,d) {
  for(i in W) {
    n+=1
    d+= d2h(NUMS[i],a[i])^2 }
  return (d/n)^.5 }

function sortByD2h(x,_,,y,__) {
  return compare(Row[x]["d2h"],Row[y]["d2h"]) }

#-----------------------------------
function compare(x,y) {
  if (x < y)  return -1
  if (x==y )  return  0
  if (x > y) return  1 }

function norm(x,lo,hi)     { return (x-lo) / (hi- lo + 1/Big) }
function abs(x)            { return x<0 ? -x : x }

function new(a,k)          { a[k][k]; delete a      }      
function has(a,k,f)        { new(a,k); @f(a[k])     }
function has1(a,k,f,g)     { new(a,k); @f(a[k],g)   }
function has2(a,k,f,g,h)   { new(a,k); @f(a[k],g,h) }
