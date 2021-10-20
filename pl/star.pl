% star.pl

% stuff to time goals

:- ensure_loaded(format).

% compare runtimes of X and Y
N*X/Y :- !, % cut needed to we dont fall 
	    % into the next clause
	% cputime: swi function
	% to grab time (in secs). 
	Start is cputime,
	times(N,X),
	Mid is cputime,
	times(N,Y),
	End is cputime,
	Ratio is (Mid - Start)/(End - Mid),
	format('~p = [~P] / [~13P]',
	       [Ratio, X,Y]).

% repeat some goal N times, show how long
% it takes to run (using the built in 'time'
% predicate).
N*X :- time(times(N,X)).

% failure drive loop to repeat N times
times(N,X) :- between(1,N,_), X, fail.
times(_,_).