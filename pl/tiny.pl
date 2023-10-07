/*

with goal-expansions commented out:

p(A, B) :-
    of(emp, A, C),
    =(name, tim, C, D),
    =(age, 10, D, E),
    >(shoesize, 20, E, B).

q(A, B) :-
    of(fred, A, C),
    =(a, 10, C, D),
    in(love, shoesize, D, E),
    =(b, 10, E, F),
    /(a, Old, F, G),
    print(Old),
    B=G.

with goal-expansions uncommented (lots of load-time optimizations)

p(emp(tim, 10, A), B) :-
    true,
    true,
    true,
    >(shoesize, 20, emp(tim, 10, A), B).

q(fred(10, A), B) :-
    true,
    true,
    in(love, shoesize, fred(10, A), fred(C, 10)),
    true,
    /(a, Old, fred(C, 10), D),
    print(Old),
    B=D.
 */

:- discontiguous slot/6.

:- op(700, xfx, :=).
:- op(700, xfx, in).

% some DCG magic
=( X,Y) --> slot(_,X,Y,Y).
:=(X,Y) --> slot(_,X,_,Y).
+( X,Y) --> slot(_,X,Z, [Y|Z]).
in(T,X) --> slot(_,X,Z,Z), {member(T,Z)}.
=<(X,Y) --> slot(_,X,Z,Z), {Z=<Y}.
>=(X,Y) --> slot(_,X,Z,Z), {Z>=Y}.
\=(X,Y) --> slot(_,X,Z,Z), {Z\=Y}.
<( X,Y) --> slot(_,X,Z,Z), {Z <Y}.
>( X,Y) --> slot(_,X,Z,Z), {Z >Y}.

%---------------------------------------------------------
% timm's tiny object language
term_expansion(X=L0, L) :- findall(Y, xpand(X=L0,Y), L).

xpand(Functor = Slots, slot(Functor,Slot,Val1,Val2,T1,T2)) :-
  length(Slots, Arity),
  length(L1,    Arity),
  length(L2,    Arity),
  T1 =.. [Functor | L1],
  T2 =.. [Functor | L2],
  xpand1(L1, L2, Slots, Slot, Val1, Val2).

xpand1([X|Vs],[Y|Vs],[S|_], S,X,Y).
xpand1([V|Xs],[V|Ys],[_|Ss],S,X,Y) :- xpand1(Xs,Ys,Ss,S, X,Y).

%---------------------------------------------------------
% define some swaps (to be done at load time)
goal_expansion(X,X)            :- predicate_property(X, built_in),!.
goal_expansion(of(F,X,X),true) :- ground(F), slot(F,_,_,_,X,X).
goal_expansion(X,true) :- 
  clause(X,slot(A,B,C,D,E,F)),
  slot(A,B,C,D,E,F).
%
%---------------------------------------------------------
% some assertions in timm's tiny object language

emp = [name,age,shoesize].

p --> of(emp), name=tim, age=10,  shoesize>20.

fred=[a,b].

q --> of(fred), a=10, love in shoesize, b=10,a/Old,{print(Old)}.

terms = [all].

:- listing(p).
:- listing(q).

