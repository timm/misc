:- discontiguous slot/6, def/2.
say(X) :- print(X), nl.

term_expansion(def(X,L0), L) :- 
  findall(Y, xpand(def1(X,L0),Y), L).

xpand(def1(Functor, Slots), slot(Functor,Slot,Val1,Val2,T1,T2)) :-
  length(Slots, Arity),
  length(Vs1,   Arity),
  length(Vs2,   Arity),
  T1 =.. [Functor | Vs1],
  T2 =.. [Functor | Vs2],
  xpand1(Vs1, Vs2, Slots, Slot, Val1, Val2).

xpand1([X|Vs],[Y|Vs],[S|_], S,X,Y).
xpand1([V|Xs],[V|Ys],[_|Ss],S,X,Y) :- xpand1(Xs,Ys,Ss,S, X,Y).

 =(Slot,Val,T,T)  :- slot(_,Slot,Val,Val,T, T).
:=(Slot,Val,T0,T) :- slot(_,Slot,_  ,Val,T0,T).
is(Slot,Is,T0,T) :- Val is Is, slot(_,Slot,_  ,Val,T0,T).

new(F,_,T) :- slot(F,_,_,_,_,T).
pipe(X,Y,X,Y).

def(num,[at,txt,mu,m2,lo,hi]).
num(At,Txt) -->
  new(num),
  at = At,
  txt= Txt,
  n  = 0,
  m2 = 0,
  hi = -1E31,
  lo =  1E31.


def(sym,[at,txt,mode,most,seen]).

inc(H, V, [],        [H=V]).
inc(H, V, [H=V0|T],  [H=V1|T]) :- !,V1 is V0+V.
inc(H, V, [H   |T0], [H   |T]) :- inc(H,V,T0,T).

:- listing(num).
:- halt.
