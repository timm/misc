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

term_expansion(X=Y, Out) :-
  fields(Y, Fields, Defaults),
  findall(Z, slot1(X, Fields, Defaults, Z), Out).

fields([],      [],     []).
fields([F=D|L], [F|Fs], [D|Ds]) :- !, fields(L,Fs,Ds).
fields([F  |L], [F|Fs], [_|Ds]) :-    fields(L,Fs,Ds).

slot1(X, _, Defaults, new(X,T)) :- T =.. [X|Defaults].
slot1(X, Fields, _, slot(X,Field,Old,New,T0,T)) :- 
  member(Field,Fields),
  slot2(Field, Old, New, Fields, L0, L),
  T0 =.. [X|L0],
  T  =.. [X|L ].

slot2(_,_,_,[],    [],    []).
slot2(F,X,Y,[F|FS],[X|L0],[Y|L]) :- !, slot2(F,X,Y,FS,L0,L).
slot2(F,X,Y,[_|FS],[Z|L0],[Z|L]) :-    slot2(F,X,Y,FS,L0,L).

o(X,A) :- o(X,_,A).
o(a  X,_,A)   :- slot(X,_,_,_,A,A).
o(an X,_,A)   :- slot(X,_,_,_,A,A).
o(X of  Y,A,B):- slot(_,X,Old,New,A,B), o(Y,Old,New).
o(X and Y)   --> o(X), o(Y).
o(X or  Y)   --> o(X) | o(Y).
o(X @=  Y/Z) --> slot(_,X,Y,Z).
o(X ++  Y)   --> slot(_,X,Z,[Y|Z]).
o(X :=  Y)   --> slot(_,X,_,Y).
o(X =   Y)   --> slot(_,X,Y,Y).
o(X in  Y)   --> slot(_,X,Z,Z), {member(Z,Y)}.
o(X =<  Y)   --> slot(_,X,Z,Z), {Z=<Y}.
o(X >=  Y)   --> slot(_,X,Z,Z), {Z>=Y}.
o(X \=  Y)   --> slot(_,X,Z,Z), {Z\=Y}.
o(X  <  Y)   --> slot(_,X,Z,Z), {Z <Y}.
o(X  >  Y)   --> slot(_,X,Z,Z), {Z >Y}.

goal_expansion(o(A,B,C),D) :- clause(o(A,B,C),D).

%------------------------------------------------
emp = [name, dob,shoesize].
num = [emp,weight].
tbl = [cols=[],rows=[]].
num = [n=0,m=0,sd].
cols= [x=[nums=[], syms=[]]
      ,y=[nums=[], syms=[], less=[], more=[], klass=_]].

z(N) --> o name of emp := N and dob < 2000 and  shoesize > 23 and name=helen.
z --> o dob in [a,b] and name := tim. 

col0(L) --> o  x of nums ++ L.
 

