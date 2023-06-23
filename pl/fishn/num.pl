% vim : filetype=prolog et ts=2 :
:- [fishn].
:- uses([field,str]).

num = [at,txt,n,mu,mu2,lo,hi].
 
init(At,Txt) -->
  for(num),
  at=At,txt=Txt, n=0, mu=0, mu2=0, ({end(Txt,"-")}  -> w= -1 ; w=1).

mid(M) for num--> fmu=M.
div(D) for num -->
  n=N,mu2=M2,  {D is sqrt((M2/(N-1)))}. 
