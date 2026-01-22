BEGIN { FS="," }
   # Internal globals:
   # Total=0    # count of all instances
   # Classes    # table of class names/frequencies
   # Freg       # table of counters for values in attributes in classes
   # Seen       # table of counters for values in attributes
   # Attributes # table of number of values per attribute

NR==1 {head()}
Pass==1 {train()}
Pass==2 {print $NF "," classify()}
END { rogues() }

function rogues(   i) { for(i in SYMTAB) if (i ~ /^[a-z]/) print("?",i) }

function head(     c) {
  for(c=1; c<=NF; c++) {
    NAME[c] = $c
    if ($c ~ /^[A-Z]/) M2[c]=MU[c]=SD[c];
    if ($c ~ /-$/) Y[c]=0
    if ($c ~ /+$/) Y[ci]=1 }}

function train(    c,y,k) {
  for(c=1; c<=NF; c++) 
    if ($c != "?" && (c in MU)) {$c += 0; update(c, $c)}
  k =  klass("y", update("y", disty()))
  Klass[k]++
  for(c=1; c<=NF; c++) if ($c != "?") F[k][c][bucket(c,$v)]++ } 

function klass(c,v) { return norm(c, v) < 1 - (1 / sqrt(N[c])) }

function update(c,v) {
  N[c]++
  d = v - MU[c]; MU[c] += d/N[c]; M2[c] += d*(v - MU[c])
  SD[c] = N[c] < 2 ? 1E-32 : sqrt(M2[c] / (N[c] - 1 ))
  return v}

function norm(c,v)   { return 1 / (1 + exp( -1.7 * (v - MU[c])/SD[c]))}
function bucket(c,v) { return (!(c in MU) || v=="?") ? v : int(BINS * norm(c,v))}

function disty(    c,d,n) {
  d=n=0
  for(c in Y) { n++; d += (norm(c,$c) - Y[c])^2 }
  return sqrt(d/n) }

function train(    i,c) { 
   Total++;
   c=$NF;
   Classes[c]++;
   for(i=1;i<=NF;i++) {
     if ($i=="?") continue;
     Freq[c,i,$i]++
     if (++Seen[i,$i]==1) Attributes[i]++}
 }

function classify(         i,temp,what,like,c) {  
   like = -100000; # smaller than any log
   for(c in Classes) {  
     temp=log(Classes[c]/Total); #uses logs to stop numeric errors
     for(i=1;i<NF;i++) {  
       if ( $i=="?" ) continue;
       temp += log((Freq[c,i,$i]+1)/(Classes[c]+Attributes[i]));
     };
     if ( temp >= like ) {like = temp; what=c}
   };
   return what;
}
