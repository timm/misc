% vim: filetype=prolog et ts=2 :
:- [fishn].
:- uses([list]).

:- dynamic field/6.
:- op(700,xfx,:=).

=(   At,V,T, T) :- field(_,At,V,    V,      T, T).
:=(  At,V,T0,T) :- field(_,At,_,    V,      T0,T).
pop( At,H,T0,T) :- field(_,At,[H|L],L,      T0,T).
push(At,X,T0,T) :- field(_,At,L,    [X|L],  T0,T).
inc( At,X,T, T) :- field(_,At,L0,   [X/N|L],T, T), (without(L0,X/N0,L) -> N is N0+1; L=L0,N=1).
in(  At,X,T, T) :- field(_,At,L0,   [X|L],  T, T), without(L0,X,L).
>=(  At,N)     --> =(At,N1), {N1 >= N}.
>(   At,N)     --> =(At,N1), {N1 >  N}.
<(   At,N)     --> =(At,N1), {N1 <  N}.
=<(  At,N)     --> =(At,N1), {N1 =< N}.

isa(F,Y) :- isa(F,Y,Y).
isa(F,Y,Y) :- field(F,_,_,_,Y,Y).

term_expansion(F = L, Fields) :- findall(Field, fields(F,L,Field), Fields).

fields(F,L,field(F,X,V0,V1,T,T)) :-
  member(X,L), field1(L,V0,V1,L0,L1,X), T=.. [F|L0], T=.. [F|L1].

field1([],    _, _,[],      [],      _).
field1([H|L],V0,V1,[V0|V0s],[V1|V1s],H) :-         field1(L,_, _, V0s,V1s,H).
field1([H|L],V0,V1,[V |V0s],[V |V1s],X) :- H \= X, field1(L,V0,V1,V0s,V1s,X).

term_expansion((A --> for(F),B), (A1 :- B1)) :-
  dcg_translate_rule((A --> tmp,B),(A1 :- tmp(X,X),B1)),
  field(F,_,_,_,X,X).
