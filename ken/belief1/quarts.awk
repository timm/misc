BEGIN {
  FS=","; 
  OFS=",	"
  T[2]="Engineering enhancement	"
  T[3]="Scientific enhancement	"
  T[4]="Testing			"
  T[5]="Bug fixing		"
}

{for(c=2;c<=NF;c++) 
   D[$1][c][++tmp] =$c
}
END{ for(t in D) 
       for(c in D[t]) {
         n=asort(D[t][c])
  
         q1 = int(.25*n+0.5)
         q2 = int(.5*n+0.5)
         q3 = int(.75*n+0.5)
         q1=  q1 ? q1 : 1
         q2= q2 ? q2 : 1
         q3= q3 ? q3 : 1
	 #print "t "t, " c "c," n "n," q1 "q1," q2 "q2," q3 "q3
         print T[c], t, D[t][c][q1], D[t][c][q2], D[t][c][q3]
    }}

