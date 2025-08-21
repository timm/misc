:- dynamic ako/3, slot/6.
:- discontiguous ako/3, slot/6.

term_expansion(F=Pairs, [ako(F,Eg,Zero)|Slots]) :- 
  length(Pairs, N), 
  functor(Eg,F,N),
  maplist(arg(2), Pairs, L), Zero =.. [F|L],
  findall(Slot, slots(F=Pairs,N,Slot), Slots).

slots(F=Pairs, N, slot(T0,F,Field,V0,V1,T1)) :-
  functor(T0,F,N), T0 =.. [_|L0],
  functor(T1,F,N), T1 =.. [_|L1],
  nth1(A, Pairs, Field=_),
  slots1(L0,L1,A,1,V0,V1).

slots1([],[],_,_,_,_).
slots1([H0|T0],[H1|T1],A,N,V0,V1) :-
  (A is N -> V0=H0,V1=H1; H0=H1), slots1(T0,T1,A,N+1,V0,V1).

%----------------------------------------------------------  
singleton(X) :- findall((X,Y),clause(X,Y),[(X,Y)]).

term_expansion((F/Head :- Body), (Head :- Body)) :- 
  ako(F,T,_), arg(1,Head,T). 

goal_expansion(slot(T0,F,Field,V0,V,T),true) :- 
  singleton(slot(   T0,F,Field,V0,V,T)).

goal_expansion(o(X,Y),   Body) :- clause(o(X,Y,Y),Body).
goal_expansion(o(X,Y,Z), Body) :- clause(o(X,Y,Z),Body).

%----------------------------------------------------------  
o(X,Y) :- o(X,Y,Y).

o([H|T], A,B) :- o(H,A,C), o(T,C,B).
o(X == Y,A,B) :- slot(A,_,X,_,Y,B).
o(X =  Y,A,A) :- slot(A,_,X,Y,Y,A).
o(X =< Y,A,A) :- slot(A,_,X,Z,Z,A), Z =< Y.
o(X >= Y,A,A) :- slot(A,_,X,Z,Z,A), Z >= Y.
o(X \= Y,A,A) :- slot(A,_,X,Z,Z,A), Z \= Y.
o(X  < Y,A,A) :- slot(A,_,X,Z,Z,A), Z  < Y.
o(X  > Y,A,A) :- slot(A,_,X,Z,Z,A), Z  > Y.

%----------------------------------------------------------  
num=[i=0, txt="", n=0, mu=0, sd=0, m2=0, lo=1e32, hi= -1e32].
sym=[i=0, txt="", has=[]].

num/new(I,M) :- o(mu>M,I), print(M).

%----------------------------------------------------------  
:- listing([slot,ako,new]).
:- halt.
