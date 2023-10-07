/*

# EXAMPLE1:

with goal-expansions commented out:

:- listing(p), listing(q).

p(emp(A, B, C), D) :-
    true,
    slot(_, name, tim, tim, emp(A, B, C), E),
    slot(_, age, 10, 10, E, F),
    slot(_, shoesize, G, G, F, H),
    G>20,
    D=H.

q(fred(A, B), C) :-
    true,
    slot(_, a, 10, 10, fred(A, B), D),
    slot(_, shoesize, E, E, D, F),
    member(love, E),
    G=F,
    slot(_, b, 10, 10, G, H),
    /(a, Old, H, I),
    print(Old),
    C=I.

*/

:- discontiguous slot/6.
:- op(1200, xfx, --->).
:- op(700, xfx, :=).
:- op(700, xfx, in).

%---------------------------------------------------------
% term accessors 
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

% some inline goal-expansions
term_expansion((X0 ---> Y0), [(X :- Y), 
                              goal_expansion(X,Y)]) :-
  dcg_translate_rule((X0 --> Y0), (X :- Y)).

% some DCG magic
=( X,Y) ---> slot(_,X,Y,Y).
:=(X,Y) ---> slot(_,X,_,Y).
+( X,Y) ---> slot(_,X,Z, [Y|Z]).
in(T,X) ---> slot(_,X,Z,Z), {member(T,Z)}.
=<(X,Y) ---> slot(_,X,Z,Z), {Z=<Y}.
>=(X,Y) ---> slot(_,X,Z,Z), {Z>=Y}.
\=(X,Y) ---> slot(_,X,Z,Z), {Z\=Y}.
<( X,Y) ---> slot(_,X,Z,Z), {Z <Y}.
>( X,Y) ---> slot(_,X,Z,Z), {Z >Y}.

% define some swaps (to be done at load time)
goal_expansion(of(F,X,X),true) :- ground(F), slot(F,_,_,_,X,X).

%---------------------------------------------------------
% some assertions in timm's tiny object language

emp = [name,age,shoesize].

p --> of(emp), name=tim, age=10,  shoesize>20.

fred=[a,b].

q --> of(fred), a=10, love in shoesize, b=10,a/Old,{print(Old)}.

terms = [all].

:- listing(p).
:- listing(q).

