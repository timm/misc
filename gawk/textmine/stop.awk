function stop(a,    s,tmp,i) {
  s= "the,be,to,of,and,a,in,that,have,I,it,for,not,on,with,he,as,you,do,at,this,but,his,by,from,"\
     "they,we,say,her,she,or,an,will,my,one,all,would,there,their,what,so,up,out,if,about,who,get,"\
     "which,go,me,when,make,can,like,time,no,just,him,know,take,people,into,year,your,good,some,could,"\
     "them,see,other,than,then,now,look,only,come,its,over,think,also,back,after,use,two,how,our,work,"\
     "first,well,way,even,new,want,because,any,these,give,day,most,us,is"
  split(s,tmp,",")
  for(i in tmp) a[tmp[i]] = tmp[i] 
}

BEGIN { OFS = FS="\n"; RS="" 
        stop(WORDS)}
      { print("",$1,$2,$3,$4)
        for(i=5;i<=NF;i++) 
          if (! WORDS[$i]) print $i }
