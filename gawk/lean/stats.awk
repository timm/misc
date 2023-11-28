BEGIN {FS=","}
NR==1 { print "#names,"$0
        for(i=1;i<=NF;i++) {
          array(Seen,i)
          Nump[i] = $i~/^[\t ]*[A-Z]/ }}
NR>1 { for(i=1;i<=NF;i++) {
          $i = (i in Nump) ? $i += 0 : trim($i)
          if ($i != "?") push(Seen[i], $i)  }}
END  { for(i in Nump) asort(Seen[i]) 
       for(i=1;i<=NF;i++)
         print mid

function trim(s) { sub(/^[ \t]*/,"",s); sub(/[ \t*]$',"",s); return s}

function push(a,x) { a[1 + length(a)] = x }

function array(a,k) { a[k][1]; del a[k][1] } 

function mode(a,    most,out) {
  for (i in a) if (a[i] > most) {most=a[i], out=i}
  return out }

function per(a,p)   { return a[int(p*#a)] }

function mid(a,i) { 
  printf(",%s",(i in Nump) ? sd(Seen[i]) : mode(Seen[i])) }
