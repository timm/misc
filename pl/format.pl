% format.pl

%--------------------------------------
% stuff to simplify printing clauses.
% swi-prolog lets a programmer customize
% the format statement.

% FIRST, a predicate of arity 2 is registerred
%        next to some letter
:- format_predicate('P',p(_,_)).

% SECOND, write the predicate.
p(default,X) :- !, p(0,X).
p(_,(X :- true)) :- !, format('~p.\n',X).
p(_,(X :- Y   )) :- !, portray_clause((X :- Y)).
p(N,[H|T]      )  :- !, not(not((numbervars([H|T],N,_),
	                     format('~p',[[H|T]])))).
p(N,X          ) :- not(not((numbervars(X,N,_),
	                     format('~p',X)))).

%--------------------------------------
% stuff to simplify right justifying text

% FIRST:
:- format_predicate('>',padChars(_,_)).

% SECOND:
padChars(default,A) :-
	padChars(5,A).
% the first arg "S" is the optional argument
% someone may have given with the "~" command
padChars(S,A) :-
	writeThing(A,Thing,N),
	Pad is S - N,
	% standard trick to emulate
	% for(i=1;i<=N;i++) { doThis }
	forall(between(1,Pad,_),put(32)),
	write(Thing).

writeThing(X,S,L) :-
	% sformat returns the string in
	% the first arg
	sformat(S,'~w',[X]), string_length(S,L).

%--------------------------------------
% stuff to simplify left justifying text

% FIRST:
:- format_predicate('<',charsPad(_,_)).

% SECOND:
charsPad(default,A) :- charsPad(5,A).
charsPad(S,A) :-
	writeThing(A,Thing,N),
	atom_length(A,N),
	Pad is S - N,
	write(Thing),
	forall(between(1,Pad,_),put(32)).

%--------------------------------------
% stuff to simplify printing N twiddles,
% scaled to some factor

% FIRST:
:- format_predicate('S',twiddle(_,_)).

% SECOND:
twiddle(default,A) :- twiddle(25,A).
twiddle(W,N) :-
	N1 is round(N/W),
	forall(between(1,N1,_),put(126)).

%--------------------------------------
% stuff to simplify printing lists
% scaled to some factor

% FIRST:
:- format_predicate('L',printL(_,_)).

% SECOND:
printL(default,List) :- printL(10^6,List). 
printL(TooLong,List) :-
	forall((nth1(Pos,List,Item),
	        Pos < TooLong),
	       format('\t~w\n',Item)).


