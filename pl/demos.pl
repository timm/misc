% demos.pl

% handle the generic demo stuff

% run the demo predicate, saving output
demos(F) :-
         sformat(Out,'~w.out',F),
	 write(Out),nl,
	 tell(Out),
	 format('% output from ~w.pl\n',F), 
         % never fail 
	 ignore(demo),
	 told.

% run through all the demo1's
demo :- demo1,fail.
demo.