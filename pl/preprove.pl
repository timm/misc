% preprove.pl

% pre-processer so we dont need
% those darn cuts in prove

% once(X) :- X,!.
preprove(X,Y) :- 
	once(preprove1(X,Y)).

% standard top-down parsing trick #1
% always check for the var case on top
preprove1(X, call(X))    :- var(X).

% standard top-down parsing trick #2
% sub-goals call the once-ed head
preprove1((X0,Y0),(X,Y)) :-
	preprove(X0,X),
	preprove(Y0,Y).
preprove1((X0;Y0),(X;Y)) :-
	preprove(X0,X),
	preprove(Y0,Y).
preprove1(\+ X0, \+ X)   :-
	preProve(X0,X).
preprove1(X,  call(X))   :-
	% swi-specific- slow!
	% should only ever call once!
	predicate_property(X,Y),
	(Y = builtin_in
        ; Y = imported_from(system)).
preprove1(X,  prove(X)).

term_expansion(+ X,X).
term_expansion((+ X :- Y0),(X :- Y)) :-
	preprove(Y0,Y).