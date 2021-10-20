% dprove.pl

% depth-first iterative deepenning
% pre-processor using a cut-less prove

:- [demos, star,preprove,elecplus].

demof :- demos(dprove).

demo1 :- listing(lit).
demo1 :- nl,
	 R=10000,
	 [elec],
	 Start1 is cputime,
	 R*(once(prolog(live(w5)))),
	 Stop1 is cputime,
	 [elecplus],
	 Start2 is cputime,	 
	 R*(once(dprove(live(w5),_))),
	 Stop2 is cputime,
	 Ratio is (Stop2 - Start2)
                  /(Stop1 - Start1),
	 format('interpreted / raw = ~w\n',
	          [Ratio]).

% just call raw prolog
prolog(X) :- X.

% shovel predicate.
% buries the real call inside a succincy
% high-level driver
dprove(X) :- dprove(X,_).

dprove(X,MaxDepth) :-
	between(1,10,MaxDepth),
	prove(prove(X),MaxDepth).

% observe the beauty of unique functors,
% possible after a one-time pre-processor
prove(true,     _).
prove(\+ X,     D) :- \+ prove(X,D).
prove((A;B),    D) :- prove(A,D); prove(B,D).
prove((A,B),    D) :- prove(A,D), prove(B,D).
prove(call(A),  _) :- A.
prove(prove(A),D0) :-
	D0 > 0,
	D is D0 - 1,
	clause(A,B),
	prove(B,D).

% odd tales of DFID
% repeats old searches
% B = xtra effort
% 2 = 4
% 3 = 2.25
% 4 = 1.77778
% 5 = 1.5625
% 6 = 1.44
% 7 = 1.36111
% 8 = 1.30612
% 9 = 1.26563
% 10 =1.23457
% with high branching factors, out-performs
% other searchers. if solution found, will be
% the shortest solution. if no solution found,
% will search on VERY deeply, not using
% much memory. so it will in a space effecient
% manner, search and search and search... .
% Hence, the call to once above.