:- discontiguous '_'/6, '_0'/2,of/2.
term_expansion((method(X/Head) --> Body),(Head --> Body)):- print(X).

term_expansion(X=L0, L) :- 
  findall(Y, xpand(X=L0,Y), L).

term_expansion((def(X0) --> Y0), 
      [(o(X) --> Y0), goal_expansion(X,Y)]) :- 
  dcg_translate_rule((X0-->Y0), (X:-Y)).

xpand(Functor = Slots, 
      '_0'(Functor,Term))  :-
  length(Slots,Arity),
  functor(Term, Functor,  Arity).

xpand(Functor = Slots, 
      '_'(Functor,Slot,Val1,Val2,T1,T2)) :-
  length(Slots, Arity),
  length(Vs1,   Arity),
  length(Vs2,   Arity),
  T1 =.. [Functor | Vs1],
  T2 =.. [Functor | Vs2],
  xpand1(Vs1,Vs2,Slots,Slot, Val1, Val2).

xpand1([X|Vs],[Y|Vs],[S|_], S,X,Y).
xpand1([V|Xs],[V|Ys],[_|Ss],S,X,Y) :- xpand1(Xs,Ys,Ss,S, X,Y).

def(of(F),  X,X) :- {'_0'(F,X)}.
def(X/   Y) --> '_'(_,X,Y,Y).
def(X/ Y/Z) --> '_'(_,X,Y,Z).
def(X +  Y) --> '_'(_,X,Z, [Y|Z]).
def(X =  Y) --> '_'(_,X,Y,Y).
def(T - X) --> '_'(_,X,Z,Z), {member(T,Z)}.
def(X =< Y) --> '_'(_,X,Z,Z), {Z=<Y}.
def(X >= Y) --> '_'(_,X,Z,Z), {Z>=Y}.
def(X \= Y) --> '_'(_,X,Z,Z), {Z\=Y}.
def(X  < Y) --> '_'(_,X,Z,Z), {Z <Y}.
def(X  > Y) --> '_'(_,X,Z,Z), {Z >Y}.

goal_expansion(of(F,X,Y),true)  :-  ground(F), o(of(F),X,Y).

emp = [name,age,shoesize].

p --> $emp, name=tim, age=10, shoesize=20.

fred=[a,b].

q --> $fred, a=10, love - shoesize, b=10,a/Old,{print(Old)}.

terms = [all].

method(a/reads(F)) --> 
  of(terms), {open(F,read,S)}, reads1(S), {close(F)}.
method(a/reads1(S)) -->
  of(terms), read(S, X), (X=end_of_file -> true | data(X), reads1(S)).


