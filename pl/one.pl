:- discontiguous slot/6, o/3.
term_expansion(X=L0, L) :- 
  findall(Y, xpand(X=L0,Y), L).

xpand(Functor = Slots, 
      slot(Functor,Slot,Val1,Val2,T1,T2)) :-
  length(Slots, Arity),
  length(Vs1,   Arity),
  length(Vs2,   Arity),
  T1 =.. [Functor | Vs1],
  T2 =.. [Functor | Vs2],
  xpand1(Vs1,Vs2,Slots,Slot, Val1, Val2).

xpand1([X|Vs],[Y|Vs],[S|_], S,X,Y).
xpand1([V|Xs],[V|Ys],[_|Ss],S,X,Y) :- xpand1(Xs,Ys,Ss,S, X,Y).

 =(Slot,Val,T,T)  :- slot(_,Slot,Val,Val,T, T).
:=(Slot,Val,T0,T) :- slot(_,Slot,_  ,Val,T0,T).

new(F,_,T) :- slot(F,_,_,_,T).

num=[at,txt,mu,m2,lo,hi].

new(num,at,txt) :-
  new(num),
  n = 0,
  m2= 0,
  hi= -1E31,
  lo=  1E31.
