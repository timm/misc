% vim : filetype=prolog ts=2 et :
:- [fishn].

runiv(X0,N,X) :- runiv(X0,0,N,X).

runiv(X,N,N,X) :- var(X),!.
runiv(X0,N0,N,X) :-
  X0 =.. L,
  member(X1,L),
  N1 is N0 + 1,
  runiv(X1,N1,N,X).

