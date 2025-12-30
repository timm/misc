% vim: ft=prolog
attrp(X) :- atom(X), sub_atom(X, _, 1, 0, '_').

half(L, Front, Back) :- half1(L, L, Front, Back).

half1([], L2, [], L2).
half1([_], [H|T], [H], T).
half1([_,_|T1], [H|T2], [H|L1], L2) :- half1(T1, T2, L1, L2).

tree([],     _).
tree([K],    t(K,_,T1,T2)).
tree([X,Y|Z],t(K,_,T0,T1)) :- half([X,Y|Z],L0,[K|L1]), tree(L0,T0), tree(L1,T1).

at(t(Key, Val,     _,      _), Key, Val) :- !.
at(t(K,     _,  Left,      _), Key, Val) :- Key @< K, at(Left,  Key, Val).
at(t(K,     _,     _,  Right), Key, Val) :- Key @> K, at(Right, Key, Val).

runiv(X,X) :- nonvar(X),!.
runiv(X0,X) :- X0 =.. [H|T], (X==H ; member(X1,T), runiv(X1,X)).

term_expansion((+Head :- Body), Asserts) :-

