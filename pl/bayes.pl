% vim: ts=2 sw=2 sts=2 expandtab: 

:- dynamic (*)/1.

:- op( 999,  fx,    o).
:- op( 998, fy,   a).
:- op( 998, fy,   an).
:- op( 997, xfy,   of).
:- op( 996, xfy,   or).
:- op( 995, xfy,  and).
:- op( 994,  fy,  not).
:- op( 800, xfx,  (@=)).
:- op( 800, xfx,  :=).
:- op( 100, xfx,   (++)).
:- op( 100, xfx,   in).

:-  (dynamic slot/6), (discontiguous slot/6).
:-  (dynamic new/2),  (discontiguous new/2).

term_expansion(F=Y, Out) :-
  fields(Y, Fields, Defaults),
  findall(Z, slot1(F, Fields, Defaults, Z), Out).

fields([],      [],     []).
fields([F=D|L], [F|Fs], [D|Ds]) :- !, fields(L,Fs,Ds).
fields([F  |L], [F|Fs], [_|Ds]) :-    fields(L,Fs,Ds).

slot1(F, _, Defaults, new(F,T)) :- T =.. [F|Defaults].
slot1(F, Fields, _, slot(F,Field,Old,New,T0,T)) :- 
  member(Field,Fields),
  slot2(Field, Old, New, Fields, L0, L),
  T0 =.. [F|L0],
  T  =.. [F|L ].

slot2(_,_,_,[],    [],    []).
slot2(F,X,Y,[F|FS],[X|L0],[Y|L]) :- !, slot2(F,X,Y,FS,L0,L).
slot2(F,X,Y,[_|FS],[Z|L0],[Z|L]) :-    slot2(F,X,Y,FS,L0,L).

o(X,A) :- o(X,_,A).

o(X of  Y, A,B) :- slot(_,X,Old,New,A,B), o(Y,Old,New).
o(X and Y, A,B) :- o(X,A,C), o(Y,C,B).
o(a  F,    A,B) :- slot(F,_,_,_,    A,B).
o(an F,    A,B) :- slot(F,_,_,_,    A,B).
o(X @= Y/Z,A,B) :- slot(_,X,Y,Z,    A,B).
o(X ++ Y,  A,B) :- slot(_,X,Z,[Y|Z],A,B).
o(X := Y,  A,B) :- slot(_,X,_,Y,    A,B).
o(X =  Y,  A,B) :- slot(_,X,Y,Y,    A,B).
o(X in Y,  A,B) :- slot(_,X,Z,Z,    A,B), member(Z,Y).
o(X =< Y,  A,B) :- slot(_,X,Z,Z,    A,B), Z=<Y.
o(X >= Y,  A,B) :- slot(_,X,Z,Z,    A,B), Z>=Y.
o(X \= Y,  A,B) :- slot(_,X,Z,Z,    A,B), Z\=Y.
o(X  < Y,  A,B) :- slot(_,X,Z,Z,    A,B), Z <Y.
o(X  > Y,  A,B) :- slot(_,X,Z,Z,    A,B), Z >Y.

goal_expansion(o(A,B,C),D) :- clause(o(A,B,C),D).
goal_expansion(slot(A,B,C,D,E,F), true) :-
  singleton(slot(A,B,C,D,E,F)).

singleton(X) :- \+ multiton(X), X.
multiton(X)  :- clause(X,_,R1), clause(X,_,R2), R1 \= R2, !.

%------------------------------------------------
emp = [name, dob,shoesize].
num = [who,weight].
tbl = [cols=[],rows=[]].
num = [n=0,m=0,sd].
cols= [x=[nums=[], syms=[]]
      ,y=[nums=[], syms=[], less=[], more=[], klass=_]].

z(N) --> o cols of name := N and dob < 2000 and  shoesize > 23.
z --> o dob in [a,b] and name := tim. 

col0(L) --> o  x of nums ++ L.
