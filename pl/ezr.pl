:- forall(member(X,[slot/6, ako/3]), ((dynamic X), (discontiguous X))).

term_expansion(F=Y, [ako(F,T,T0)|Out]) :- 
  fields(Y, Fs, Ds,Vs), 
  T0 =.. [F|Ds],
  T  =.. [F|Vs],
  findall(Z, slot1(F, Fs, Z), Out).

fields([], [], [],[]).
fields([F=D|L], [F|Fs], [D|Ds],[_|Vs]) :- fields(L,Fs,Ds,Vs).

slot1(F, Fs, slot(F,Field,Old,New,T0,T)) :- 
  member(Field,Fs),
  slot2(Field, Old, New, Fs, L0, L),
  T0 =.. [F|L0],
  T  =.. [F|L ].

slot2(_,_,_,[],    [],    []).
slot2(F,X,Y,[F|FS],[X|L0],[Y|L]) :- !, slot2(F,X,Y,FS,L0,L).
slot2(F,X,Y,[_|FS],[Z|L0],[Z|L]) :-    slot2(F,X,Y,FS,L0,L).

xpand(F=Pairs, slot(F,Field,D,V0,V1,T0,T1)) :-
  length(Pairs,N),
  nth1(M,Pairs,Field=D),
  functor(T0,F,N),
  functor(T1,F,N),
  between(1,N,A),
  xpand1(1,A, T0,V0,T1,V1).

%xpand1(1,A,T0,V0,T1,V1) :- arg(N,T0,N>A,!.
  
:- ignore(( xpand(emp=[a=1,b=2,c=3],X),print(X),nl,fail)).

o(X,Y) :- o(X,Y,Y).

o(X == Y, A, B) :- slot(_,X,_,Y,A,B).
o(X =  Y, A, A) :- slot(_,X,Y,Y,A,A).
o(X =< Y, A, A) :- o(X=Z, A), Z =< Y.
o(X >= Y, A, A) :- o(X=Z, A), Z >= Y.
o(X \= Y, A, A) :- o(X=Z, A), Z \= Y.
o(X  < Y, A, A) :- o(X=Z, A), Z  < Y.
o(X  > Y, A, A) :- o(X=Z, A), Z  > Y.

singleton(X) :- findall((X,Y),clause(X,Y),[(X,Y)]).

term_expansion((F / Head :- Body), (Head :- Body)) :- arg(1,Head,T), ako(F,T,_).

goal_expansion(slot(F,Field,V0,V,T0,T),true) :- singleton(slot(F,Field,V0,V,T0,T)).

goal_expansion(o(X,Y), Body) :- clause(o(X,Y,Y),Body).
goal_expansion(o(X,Y,Z), Body) :- clause(o(X,Y,Z),Body).

num = [i=0, txt="", n=0, mu=0, sd=0, m2=0, lo=1e32, hi = -1e32].
sym = [i=0, txt="", has=[]].

num/new(I,M) :- o(mu>M,I), print(M).

:- listing([o,new]).
:- halt.
