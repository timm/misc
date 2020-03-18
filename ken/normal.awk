BEGIN {FS =","}
{for(col=1;col<=NF;col++) data[col][++nn] = $col}

END {for (col in data) {
        n=asort(data[col])
        lo = data[col][1]
        hi = data[col][n]
        for(j in data[col])
            data[col][j] = (data[col][j] - lo)/(hi - lo)
     }
     for(row=1;row<=n;row++) {s = sep = ""
       for(col=1;col<=length(data);col++) {
         val = data[col][row]
         val = col==1 ? int(int(val*100)/10)*10 : val 
         s= s sep val
         sep=","
       }
       print s
     }
#data[col]
       
 #       q1 = int(.25*n)
  #      q2 = int(.5*n)
   #     q3 = int(.75*n)
    #    print col, data[col][q1], data[col][q2], data[col][q3]
    #}
}
     



