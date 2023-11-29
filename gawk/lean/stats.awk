@include "lib"

BEGIN {FMTS="%.2g"; FS=","}

NR==1 { print "#names,"$0
        for(i=1;i<=NF;i++) Nump[i] = $i~/^[\t ]*[A-Z]/ }

NR>1 { for(i=1;i<=NF;i++) 
         i in Nump ? Nums[i][NR]= ($i += 0): Syms[i][trim($i)]++ }

END  { for(i in Nums) asort(Nums[i]) 
       printf("#mid")
       for(i=1;i<=NF;i++) printf(",%s", (i in Nums ? median(Nums[i]) : mode(Syms[i])))
       printf("\n#sd")
       for(i=1;i<=NF;i++) printf(",%s", (i in Nums ? sd(Nums[i])     : ent(Syms[i])))
       print("") }
