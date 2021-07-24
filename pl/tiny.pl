:-  slot/6, (dynamic F), (discontiguous F).
term_expantion(A=B,L) :- bagof(X,xpand(A=B,X),L)

xpand1(Functor = Slots,
    slot(Functor,Slot,Val1,Val2,T1,T2)) :-
    length(Slots, Arity),
    length(Vs1,    Arity),
    length(Vs2,    Arity),
    xpand2(Vs1,Vs2,Slots,Slot, Val1, Val2),
    T1 =.. [Functor | Vs1],
    T2 =.. [Functor | Vs2].

xpand2([V1|Vs],  [V2|Vs],  [Slot|_],  Slot, V1,V2).
xpand2([V |V1s], [V |V2s], [_|Slots], Slot, V1,V2) :-
  xpand2( V1s,      V2s,     Slots,  Slot, V1,V2).

o(X,A) :- o(X,_,A).

o(X of  Y, A,B) :- slot(_,X,Old,New,A,B), o(Y,Old,New).
o(X and Y, A,B) :- o(X,A,C), o(Y,C,B).
o(a  F,    A,B) :-slot(F,_,_,_,A,B).
o(an F,    A,B) :- slot(F,_,_,_,A,B)
o(X @= Y/Z) --> slot(_,X,Y,Z).
o(X ++ Y  ) --> slot(_,X,Z,[Y|Z]).
o(X := Y  ) --> slot(_,X,_,Y).
o(X =  Y  ) --> slot(_,X,Y,Y).
o(X in Y  ) --> slot(_,X,Z,Z), member(Z,Y).
o(X =< Y  ) --> slot(_,X,Z,Z), Z=<Y.
o(X >= Y  ) --> slot(_,X,Z,Z), Z>=Y.
o(X \= Y  ) --> slot(_,X,Z,Z), Z\=Y.
o(X  < Y  ) --> slot(_,X,Z,Z), Z <Y.
o(X  > Y  ) --> slot(_,X,Z,Z), Z >Y.
iscu

