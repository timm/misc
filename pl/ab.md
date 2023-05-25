/*

# Abduction 101    
Tim Menzies   
`timm@ieee.org`

## System

#### Background knowledge*/

     happy/t        :- rich/t,healthy/t.
     happy/t        :- tranquility /hi.
     tranquility/hi :- conscience/clear.
     tranquility/hi :- satiated/t.
     satiated/t     :- diet/fatty.
     healthy/t      :- diet/light. /*

###  Current facts. */

     rich/t.
 
     %tim/f.
     %satiated/f.  /*

### High level goal */

     todo/t        :- happy/t. /*

## Output

Here's one run. 
 
     ?- goes.
     % assumptions
     0* (diet/light)
     1* (rich/t)
     2* (happy/t)
     2* (healthy/t)
     2* (todo/t)
     
     % assumptions
     0* (conscience/clear)
     2* (happy/t)
     2* (todo/t)
     2* (tranquility/hi)
     
     % assumptions
     0* (diet/fatty)
     2* (happy/t)
     2* (satiated/t)
     2* (todo/t)
     2* (tranquility/hi)

Here's anohter run. Hmmm... different order. Why?

     ?- goes.
     % assumptions
     0* (diet/light)
     1* (rich/t)
     2* (happy/t)
     2* (healthy/t)
     2* (todo/t)
     
     % assumptions
     0* (diet/fatty)
     2* (happy/t)
     2* (satiated/t)
     2* (todo/t)
     2* (tranquility/hi)
     
     % assumptions
     0* (conscience/clear)
     2* (happy/t)
     2* (todo/t)
     2* (tranquility/hi)
 
## Implementation

### Working memory management */
 
     reset  :-
     	dynamic(assuption/3),
     	retractall(assumption(_,_,_)).
     
     report :- write('% assumptions\n'), report1, fail.
     report :- nl.
     
     report1 :-
     	setof(Z*(X/Y),assumption(X,Y,Z),All),
     	member(One,All),
     	write(One),
     	nl. /*


## Standard stuff */
 
     goes :- go, fail.
     goes.
     
     goes(X) :- go(X),fail.
     goes(_).
     
     go    :- reset, run,    report.
     go(X) :- reset, run(X), report.
     
     run    :- maybe(todo/t).
     run(X) :- maybe(X). /*

## Assumption management */
 
     assume(X/Y,_)   :- assumption(X,Z,_),!,Y=Z.
     assume(X/Y,How) :- bassert(assumption(X,Y,How)).
     
     bassert(X) :- assert(X).
     bassert(X) :- retract(X),fail. /*

## Assumption generation */
 
     maybe(X) :- once(maybe0(X,Y)), maybe1(Y).
     
     maybe0((X,Y),      (X,Y)).
     maybe0(X/Y,        fact(X/Y))      :- clause(X /_,true).
     maybe0(X/Y,        rule(X/Y))      :- clause(X/_,_).
     maybe0(X/Y,        abducible(X/Y)).
     
     maybe1((X,Y))        :- maybe(X),maybe(Y).
     maybe1(abducible(X)) :- assume(X,0).
     maybe1(fact(X))      :- assume(X,1), one(X).
     maybe1(rule(X))      :- assume(X,2), one(clause(X,Y)), maybe(Y). /*

## Random ordering

Return solutions to goal C<X>, in some random order. */
 
     one(X) :-
     	setof(N/X,(X, N is random(10000)), All),
     	member(_/X,All).

     :- print(100),goes, halt.

