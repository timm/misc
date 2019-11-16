% joining partial eval to full term
%
:- dynamic (*)/1.

:- op(1200, fx,     #).
:- op( 999, xfx,   (?=)).
:- op( 998, xfx,   if).
:- op( 997, xfx, then).
:- op( 996, xfy,   or).
:- op( 995, xfx,  and).
:- op( 994,  fy,  not).
:- op( 993,  fx,  (*)).
:- op( 804, xfy, (::)).
:- op( 703, xfx,   of).
:- op( 702, xfx,  now).
:- op( 100, xfx,   in).
:- op(   2,  fx, rule).
:- op(   1, xfx,  for).

%---------------------------------------------------
%  term accessors

:- F= has/2, (dynamic F), (discontiguous F).
more(Functor = Fields, 
     has(Functor,Field,Val1,Val2,Term1,Term2)) :-
    length(Fields, Arity),
    length(Vs1,    Arity), 
    length(Vs2,    Arity), 
    args(Vs1,Vs2,Fields,Field, Val1, Val2),
    Term1 =.. [Functor | Vs1],
    Term2 =.. [Functor | Vs2].

args([V1|Vs],  [V2|Vs],  [Field|_],  Field, V1,V2).
args([V |V1s], [V |V2s], [_|Fields], Field, V1,V2) :-
        args(V1s, V2s,     Fields,  Field, V1,V2). 

% shell to walk over has/4
F := T :: X :- F=T :: X, T.
F  = T :: X :- with(X, F, T).

with(X :: Y, F, T) :- with(X, F, T), with(Y, F, T).
with(X =  Y, F, T) :- has(F, X, Y, _,T,_).

% macro exansion hooks.
term_expansion(A =B,        Cs) :- bagof(C, more(A = B,C), Cs).
goal_expansion(F:=T :: X,    T) :- F=T :: X.
goal_expansion(F =T :: X, true) :- F=T :: X.

%term_expansion((# X0 :- Y),(X :- D,Y)) :-
%	L0 =.. X0,
%	defaults(L0, L,[],D0),
%	L =.. X,
%	defaults1(D0,D).
%
%defaults([],[],L,L
%defaults([H|T0],[H|T],     L0,L) :- var(H), defaults(T0,T,L0,L).
%defaults([H ?= V|T0],[H|T],L0,L) :- H=..[Name|V], defaults(T0,T,[L0,L]).
%
%-------------------------------------------------
% terms with names slots

rule = [spec,id,task,condition,action,vars].

%-------------------------------------------------
% Exanding rules with some extra details.

term_expansion( rule Id for Task if If then Then, Rule) :-
  term_variables(( Id,Task,If ), V1),
  term_variables(Then, V2),
  shares(V1, V2, V),
  prims(If, Specificity),
  rule=Rule :: spec=Specificity :: id=Id 
            :: vars=V           :: task=Task 
	    :: condition=If     :: action=Then.

% returns variables shared by two lists
shares([], _, []).
shares([H | T1], L, [H | T2]) :- shares1(L, H), !, shares(T1, L, T2).
shares([_ | T1], L,      T2 ) :- shares(T1, L, T2 ).

shares1([H|_],X) :- H == X. 
shares1([_|T],X) :- shares1(T,X).

% count primitives
prims(X,N) :- prims1(X,N0), N is -1*N0.

prims1(A and B,M+N) :-  !,prims1(A,M), prims1(B,N).
prims1(A or  B,M+N) :-  !,prims1(A,M), prims1(B,N).
prims1(not A,  N)   :-    prims1(A,N).
prims1(_,1).

%-------------------------------------------------
% inference

:- dynamic done/3.
thinks :- 
	retractall(done(_,_,_)), 
	* order of tasks = Tasks,
	maplist(think, Tasks),
	listing(*).

think(Task) :-
  	setof( Rule, match(Task,Rule), Rules ), !,
  	selects(Rules, Then, Done),
  	asserta( Done ),
  	Then,
  	think(Task).
think(_).

match(Task, Rule) :-
	rule := Rule :: task = Task :: id = Id 
	             :: vars = V    :: condition = If,
        If,
        not( done( Task,Id,V ) ).

selects([ Rule | _ ], Then, done(Task,Id,V) ) :-
	rule = Rule :: action=Then :: task=Task
	            :: id=Id       :: vars=V.

%-------------------------------------------------

% facts
* order of tasks = [init, elaborate, report].
* age of tim  =  20.
* age of john = 300.
* age of jane =   5.

%%%%%
% rules

rule 1 
for  init
if   age of Who > 100 
then status of Who now dead. 

rule 2      
for  init 
if   Who in [tim, john] and
     age of Who > 10
then job of Who now working.

xxx(rule 2      
for  init 
if   Who in [tim, john] and
     age of Who > 10
then job of Who now working).


%-------------------------------------------------

X and Y      :- X, Y.
X or _       :- X.
_ or Y       :- Y.
X in Y       :- member(X,Y).
X of Y =   Z :- * X of Y = Z.
X of Y >=  Z :- * X of Y = Z0, Z0 >= Z.
X of Y =<  Z :- * X of Y = Z0, Z0 =< Z.
X of Y \=  Z :- * X of Y = Z0, Z0 \= Z.
X of Y <   Z :- * X of Y = Z0, Z0 <  Z.
X of Y >   Z :- * X of Y = Z0, Z0 >  Z.
X of Y now Z :-
  retract(* X of Y = _) -> assert(* X of Y = Z);  asserta(* X of Y = Z).

%-------------------------------------------------
:- listing(has).
%
%:- thinks, halt.
