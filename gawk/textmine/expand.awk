BEGIN { FS=","; OFS="\n" }

NR>1{ 
  print("",$NF,$1,$(NF-2),$(NF-1))
  for(i=2;i<=NF-3;i++) words($i) }

function words(s,   a,i) {
  split(s,a," ")
  for(i in a) {
    a[i] = prep(a[i])
    if (a[i]) print a[i] }}

function prep(s) {
  gsub(/[^A-Za-z]/," ",s)
  sub(/^[ \t]*/,"",s)
  sub(/[ \t]*$/,"",s)
  return tolower(s) }
