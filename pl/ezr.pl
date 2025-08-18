:- forall(member(X,[slot/6, ako/2]),
          ((dynamic X), (discontiguous X))).

term_expansion(F=Y, Out) :- 
  fields(Y, Fs, X), findall(Z, slot1(F, Fs, X, Z), Out).

fields([], [], []).
fields([F=D|L], [F|Fs], [D|Ds]) :- fields(L,Fs,Ds).

slot1(F, _, Inits, ako(F,T)) :- T =.. [F|Inits].
slot1(F, Fs, _, slot(F,Field,Old,New,T0,T)) :- 
  member(Field,Fs),
  slot2(Field, Old, New, Fs, L0, L),
  T0 =.. [F|L0],
  T  =.. [F|L ].

slot2(_,_,_,[],    [],    []).
slot2(F,X,Y,[F|FS],[X|L0],[Y|L]) :- !, slot2(F,X,Y,FS,L0,L).
slot2(F,X,Y,[_|FS],[Z|L0],[Z|L]) :-    slot2(F,X,Y,FS,L0,L).

o(X,A) :- o(X,_,A).

o([H|T]) --> o(H), o(T).
o(X == Y, A,B) :- slot(_,X,_,Y,A,B).
o(X =  Y, A,B) :- slot(_,X,Y,Y,A,B).
o(X =< Y, A,B) :- slot(_,X,Z,Z,A,B), Z =< Y.
o(X >= Y, A,B) :- slot(_,X,Z,Z,A,B), Z >= Y.
o(X \= Y, A,B) :- slot(_,X,Z,Z,A,B), Z \= Y.
o(X  < Y, A,B) :- slot(_,X,Z,Z,A,B), Z  < Y.
o(X  > Y, A,B) :- slot(_,X,Z,Z,A,B), Z  > Y.

goal_expansion(ako(A,B),ako(A,B)) :- var(A) -> true | ako(A,B).

num = [i=0, txt="", n=0, mu=0, sd=0, m2=0, lo=1e32, hi = -1e32].
sym = [i=0, txt="", has=[]].

:- listing([slot,ako]).
:- halt.
