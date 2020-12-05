<img align=right src="https://www.iconexperience.com/_img/v_collection_png/256x256/shadow/keys.png">    

# Keys = cluster, discretize, contrast   

![](https://img.shields.io/badge/platform-osx%20,%20linux-lightgrey?style=flat-square)    
![](https://img.shields.io/badge/language-lua,bash-blue?style=flat-square)  
![](https://img.shields.io/badge/purpose-ai%20,%20se-blueviolet?style=flat-square)  
![](https://img.shields.io/badge/language-lua-red?style=flat-square)  
![](https://img.shields.io/badge/license-mit-green?style=flat-square)  
[lib](docs/lib.html) :: [tbl](docs/tbl.html)   



## Keys 1010

Repeat the following until happy or bored. 
Useful defaults for this algorithm are C,N,X,Z=20,100,20,2.  

-  Create N pairs of <inputs,outputs>  by either selecting 
     from a database or running a simulator or asking an oracle.  
-  Cluster on the pairs on the output scores into groups of size .5X%. . 
     If there is only one score, just sort on that one output variable.  
     Else, cluster the pairs using the output scores with (say) a 
     recursive KMEANS++ algorithm with k=2.   
-  Using the clusters, divide the N pairs into  the X% best (B) 
     and Y=100-X% worst (W outputs.   
-  Use something simple like C% equal percentile chops or Chi-merge 
     to divide numeric inputs into ranges R1, R2, etc.   
-  Take all those ranges, and all the ranges of non-numeric inputs R10,R11,
     etc and count  how often they appear among the best and worst pairs. 
     Normalize those counts  as follows: #B=#B/(N*X/100) and #W=#W/(N*Y/100)
-  Discard unpromising ranges; i.e. if  #W >= #B.    
     Sort the remaining ranges by #B^2/(#B+#W) into a list L of size S
-  Generate N’ new inputs by,  N times, using inputs L[0:max(1,int(S\*rand()<sup>Z</sup>))]  
     (and randomly selected items for everything else).  To create the new output 
     scores either ask some oracle or re-run the simulator (if it exists) or 
     interpolate between nearest neighbors in the database. 
-  Return the final best X% group.



