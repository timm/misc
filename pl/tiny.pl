:- discontiguous slot/6, '_0'/2,o/3.
%term_expansion((method(X/Head) --> Body),(Head --> Body)):- print(X).

term_expansion(X=L0, L) :- findall(Y, xpand(X=L0,Y), L).

xpand(Functor = Slots, slot(Functor,Slot,Val1,Val2,T1,T2)) :-
  length(Slots, Arity),
  length(Vs1,   Arity),
  length(Vs2,   Arity),
  T1 =.. [Functor | Vs1],
  T2 =.. [Functor | Vs2],
  xpand1(Vs1,Vs2,Slots,Slot, Val1, Val2).

xpand1([X|Vs],[Y|Vs],[S|_], S,X,Y).
xpand1([V|Xs],[V|Ys],[_|Ss],S,X,Y) :- xpand1(Xs,Ys,Ss,S, X,Y).

o(X/   Y) --> slot(_,X,Y,Y).
o(X/ Y/Z) --> slot(_,X,Y,Z).
o(X +  Y) --> slot(_,X,Z, [Y|Z]).
o(X *  Y) --> slot(_,X,Y,Y).
o(T -  X) --> slot(_,X,Z,Z), {member(T,Z)}.
o(X =< Y) --> slot(_,X,Z,Z), {Z=<Y}.
o(X >= Y) --> slot(_,X,Z,Z), {Z>=Y}.
o(X \= Y) --> slot(_,X,Z,Z), {Z\=Y}.
o(X  < Y) --> slot(_,X,Z,Z), {Z <Y}.
o(X  > Y) --> slot(_,X,Z,Z), {Z >Y}.

goal_expansion(of(F,X,X),true) :- ground(F), slot(F,_,_,_,X,X).

emp = [name,age,shoesize].

p --> of(emp), name=tim, age=10, shoesize=20.

fred=[a,b].

q --> of(fred), a=10, love - shoesize, b=10,a/Old,{print(Old)}.

terms = [all].

true(X,X).
=(X,X,Y,Y).

reads(F) --> 
  of(terms), {open(F,read,S)}, reads1(S), {close(F)}.
reads1(S) :-
  of(terms),
  read(S, X), 
  (X=end_of_file -> true | data(X), reads1(S)).

read(S,X,A,A) :- read(S,X).

data(_,_,_).

:- listing(o).
