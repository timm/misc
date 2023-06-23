% vim: filetype=prolog et ts=2 :
:- [fishn].

without([X|T],X,T).
without([H|T0],X,[H|T]) :- without(T0,X,T).


