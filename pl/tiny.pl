:- op(797,xfx,in).

:- discontiguous '_'/6.
term_expansion(A=B,L) :- bagof(X,xpand(A=B,X),L).
term_expansion((def(X0) --> Y0), goal_expansion(X,Y)) :-
  dcg_translate_rule((X0 --> Y0), (X :- Y)).

xpand(Functor = Slots, '_'(Functor,Slot,Val1,Val2,T1,T2)) :-
  length(Slots, Arity),
  length(Vs1,   Arity),
  length(Vs2,   Arity),
  T1 =.. [Functor | Vs1],
  T2 =.. [Functor | Vs2],
  xpand1(Vs1,Vs2,Slots,Slot, Val1, Val2).

xpand1([X|Vs],[Y|Vs],[S|_], S,X,Y).
xpand1([V|Xs],[V|Ys],[_|Ss],S,X,Y) :- xpand1(Xs,Ys,Ss,S, X,Y).

def($F   )  --> '_'(F,_,_,_).
def(X/   Y) --> '_'(_,X,Y,Y).
def(X/ Y/Z) --> '_'(_,X,Y,Z).
def(X +  Y) --> '_'(_,X,Z, [Y|Z]).
def(X =  Y) --> '_'(_,X,Y,Y).
def(T in X) --> '_'(_,X,Z,Z), {member(T,Z)}.
def(X =< Y) --> '_'(_,X,Z,Z), {Z=<Y}.
def(X >= Y) --> '_'(_,X,Z,Z), {Z>=Y}.
def(X \= Y) --> '_'(_,X,Z,Z), {Z\=Y}.
def(X  < Y) --> '_'(_,X,Z,Z), {Z <Y}.
def(X  > Y) --> '_'(_,X,Z,Z), {Z >Y}.

emp = [name,age,shoesize].

p --> $emp, name=tim, age=10, shoesize=20.

fred=[a,b].

q --> $fred, a=10, love in shoesize, b=10,a/Old,{print(Old)}.

terms = [all].

reads(F) --> 
  $terms,!, {open(F,read,S)}, reads1(S), {close(F)}.
reads1(S) -->
  $terms,!, read(S, X), (X=end_of_file -> true | data(X), reads1(S)).


