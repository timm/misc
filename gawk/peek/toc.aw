# vim: ft=awk :

            { Body=Body "\n" $0 }
END         { print Body }
/^# /       { print "- ["title($0)"](#"            link($0)")" help()}
/^## /      { print "  - ["title($0)"](#"          link($0)")" help()}
/^### /     { print "    - ["title($0)"](#"        link($0)")" help()}
/^#### /    { print "      - ["title($0)"](#"      link($0)")" help()}
/^##### /   { print "        - ["title($0)"](#"    link($0)")" help()}
/^###### /  { print "          - ["title($0)"](#"  link($0)")" help()}
/^####### / { print "           - ["title($0)"](#" link($0)")" help()}


function link(x)  { 
  x=tolower(x)
  gsub(/[^a-z0-9_]/,"",x); 
  return trim(x) }

function title(x) { 
  gsub(/^#+[ \t]+/,"",x); 
  return trim(x) }

function help()   { 
  getline; 
  Body=Body"\n" $0; 
  return $0 ~ /^[ \t]*$/ ? "" : " : " $0 }

function trim(x)  { 
  gsub(/^[ \t]*/,"",x); 
  gsub(/[ \t]*$/,"",x); i
  return x }


