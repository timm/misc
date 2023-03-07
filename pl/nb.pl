the([file='../../tested/etc/data/auto93.csv']).

is(X,Y) :- the(L), once(member(X=Y,L)).

:- discontiguous slot/6, def/2.
term_expansion(def(T), L) :- 
  findall(Y, xpand(def1(T),X), L).

xpand(def1(Term), slot(Functor,Slot,Arity,Val1,Val2,T1,T2)) :-
  functor(Term, Functor, Arity),
  length(Vs1, Arity),
  length(Vs2, Arity),
  T1 =.. [Functor | Vs1],
  T2 =.. [Functor | Vs2],
  xpand1(Vs1, Vs2, Slots, Slot, Val1, Val2).

xpand1([X|Vs],[Y|Vs],[S|_], S,X,Y).
xpand1([V|Xs],[V|Ys],[_|Ss],S,X,Y) :- xpand1(Xs,Ys,Ss,S, X,Y).

 new(F,_,T) :- slot(F,_,_,_,_,_,T).

def(num(n,at,txt,mu,m2,lo,hi)).
def(sym(n,at,txt,has,mode,most)).
def(cols(all,names,x,y)).

=(Functor/Slot=V)  -->  slot(Functor,Slot,_,_,V).
==(Functor/Slot=V) -->  slot(Functor,Slot,_,V,V).

on(Slot,V,L) :- member(Slot=V,L).
on(Slot,V,W,L) :- on(Slot,V,W,L0,L).
on(Slot,V,W,[Slot=V|T],[Slot=W|T]).
on(Slot,V,W,[H|T0],[H|T]) :- on(Slot,V,W,T0,T).
 
go(F) :-
  csv_read_file(F,[Names|Rows]),

cols(Names,Cols) :- cols1([names=[],x=[],y=[],all=[]], Cols) 
cols([],_,[]).


