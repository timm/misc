BEGIN { 
 FS=","; OFS="\n"  
 stop(STOPWORDS) }

{title = $1
 year  = $(NF-2)
 url   = $(NF-1)
 klass = $NF
 tmp   = $2
 for(i=3;i<=NF-3;i++) tmp = tmp " " $i
 gsub(/[^A-Za-z]/ ," " ,tmp)
 gsub(/[ ][ ]*/   ," " ,tmp)
 gsub(/^ /        ,""  ,tmp)
 print(klass,title,year,url,words(txt),"") 

}
function stop(a,    s,tmp,i) {
  s= "the,be,to,of,and,a,in,that,have,I,it,for,not,on,with,he,as,you,do,at,this,but,his,by,from,"\
     "they,we,say,her,she,or,an,will,my,one,all,would,there,their,what,so,up,out,if,about,who,get,"\
     "which,go,me,when,make,can,like,time,no,just,him,know,take,people,into,year,your,good,some,could,"\
     "them,see,other,than,then,now,look,only,come,its,over,think,also,back,after,use,two,how,our,work,"\
     "first,well,way,even,new,want,because,any,these,give,day,most,us"
  split(s,tmp,",")
  for(i in tmp) {print(tmp[i](; a[tmp[i]] = tmp[i] } }

function a2s(a,     i,sep,s) {
  for(i=1;i<=length(a);i++) {s= s sep a[i]; sep=" "}
  return s }

function push(a,x) { a[1 + length(a)] = x; return x }

function words(s,b,     a,i) {
  split(s,a," ")
  for(i in a) {
    a[i] = tolower(a[i])
    print(a[i])
    if (!(a[i] in STOPWORDS)) push(b, porter(a[i])) }
  return a2s(b) }

function porter(word,     step1a, step1b, step1c, step2, step3, step4, step5, stem) {
    stem = word

    # Step 1a
    if (sub(/sses$/, "ss", stem)) ;
    else if (sub(/ies$/, "i", stem)) ;
    else sub(/s$/, "", stem)

    # Step 1b
    if (sub(/eed$/, "ee", stem)) ;
    else if (sub(/(ed|ing)$/, "", stem) && match(stem, /[aeiou]/)) {
        if (sub(/at$/, "ate", stem)) ;
        else if (sub(/bl$/, "ble", stem)) ;
        else if (sub(/iz$/, "ize", stem)) ;
        else if (substr(stem, length(stem)-1, 1) == substr(stem, length(stem), 1))
            stem = substr(stem, 1, length(stem)-1)
        else if (match(stem, /^[^aeiou][aeiou][^aeiou]$/))
            stem = stem "e"
    }

    # Step 1c
    if (sub(/y$/, "i", stem) && match(stem, /[aeiou]/)) ;

    # Step 2
    sub(/ational$/, "ate", stem)
    sub(/tional$/, "tion", stem)
    sub(/enci$/, "ence", stem)
    sub(/anci$/, "ance", stem)
    sub(/izer$/, "ize", stem)
    sub(/abli$/, "able", stem)
    sub(/alli$/, "al", stem)
    sub(/entli$/, "ent", stem)
    sub(/eli$/, "e", stem)
    sub(/ousli$/, "ous", stem)
    sub(/ization$/, "ize", stem)
    sub(/ation$/, "ate", stem)
    sub(/ator$/, "ate", stem)
    sub(/alism$/, "al", stem)
    sub(/iveness$/, "ive", stem)
    sub(/fulness$/, "ful", stem)
    sub(/ousness$/, "ous", stem)
    sub(/aliti$/, "al", stem)
    sub(/iviti$/, "ive", stem)
    sub(/biliti$/, "ble", stem)

    # Step 3
    sub(/icate$/, "ic", stem)
    sub(/ative$/, "", stem)
    sub(/alize$/, "al", stem)
    sub(/iciti$/, "ic", stem)
    sub(/ical$/, "ic", stem)
    sub(/ful$/, "", stem)
    sub(/ness$/, "", stem)

    # Step 4
    sub(/al$/, "", stem)
    sub(/ance$/, "", stem)
    sub(/ence$/, "", stem)
    sub(/er$/, "", stem)
    sub(/ic$/, "", stem)
    sub(/able$/, "", stem)
    sub(/ible$/, "", stem)
    sub(/ant$/, "", stem)
    sub(/ement$/, "", stem)
    sub(/ment$/, "", stem)
    sub(/ent$/, "", stem)
    sub(/ou$/, "", stem)
    sub(/ism$/, "", stem)
    sub(/ate$/, "", stem)
    sub(/iti$/, "", stem)
    sub(/ous$/, "", stem)
    sub(/ive$/, "", stem)
    sub(/ize$/, "", stem)
    # Step 5
    sub(/e$/, "", stem)
    if (length(stem) > 1 && substr(stem, length(stem), 1) == "l" && substr(stem, length(stem)-1, 1) == "l")
        stem = substr(stem, 1, length(stem)-1)
    return stem
}
