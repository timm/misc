% vim : ft=prolog :

:- op(998,xfy, has ).
:- op(701,xfx, in ).

:- discontiguous has/5, isa/2.

xpand(def(F,Has), has(X,V0,V1,Term0,Term1)) :-
  member(X=_,Has),
  args(Has,V0,V1,L0,L1,X),
  Term0 =.. [F|L0],
  Term1 =.. [F|L1].
xpand(def(F,Has), isa(F,Term)) :-
  length(Has,A),
  functor(Term,F,A).

args([],      _, _, [],      [],      _).
args([H=_|Has],V0,V1,[V0|V0s],[V1|V1s],H) :-       args(Has,_, _, V0s,V1s,H).
args([H=_|Has],V0,V1,[V |V0s],[V |V1s],X) :- H\=X, args(Has,V0,V1,V0s,V1s,X).

term_expansion(T = L,Xs) :- bagof(X,xpand(def(T,L),X) ,Xs).

with(T=Out,With) :- isa(T,Term), o(With,Term,Out).
:-op(999,xfx,with).
o(X,T) :- o(X,_,T).

o(X has Y,T0,T) :- has(X,Z0,Z,T0,T), o(Y,Z0,Z).

o((X ,Y)) --> o(X), o(Y).
o(X =  Y) --> has(X,Y,Y).
o(X := Z) --> has(X,_,Z).
o(X +  Z) --> has(X,Y0,Y), {Y is Y0+Z}.
o(X -  Z) --> has(X,Y0,Y), {Y is Y0-Z}.
o(X /  Z) --> has(X,Y0,Y), {Y is Y0/Z}.
o(X *  Z) --> has(X,Y0,Y), {Y is Y0*Z}.
o(X := Z) --> has(X,_,Z).
o(X in Z) --> has(X,Y,Y), {member(Y,Z)}.
o(X >= Z) --> has(X,Y,Y), {Y >= Z}. 
o(X >  Z) --> has(X,Y,Y), {Y >  Z}. 
o(X \= Z) --> has(X,Y,Y), {Y \= Z}. 
o(X <  Z) --> has(X,Y,Y), {Y  < Z}. 
o(X =< Z) --> has(X,Y,Y), {Y =< Z}. 

person = [gender=f, fname='',lname=''].
job    = [title=student, where=raleigh,salary=0].
emp    = [who=_, job=_].

test(1) :-
   person = P with (fname=tim,lname=menzies,gender=m),
   job    = J with where=newYork,
   emp    = E with (who=P,job=J),
   print(E),nl,
   emp    = E with who has gender=m,
   print(m),nl.
