mid(L1,L2) :- mid1(L1,L1,L2).

mid1([],L,L).
mid1([_], [_|L], L).
mid1([_,_|T1], [_|T2], T3) :- mid1(T1,T2,T3).
half(L, Front, Back) :- half1(L, L, Front, Back).

half1([],       L2,     [],     L2).
half1([_],      [H|T],  [H],    T).
half1([_,_|T1], [H|T2], [H|L1], L2) :- half1(T1, T2, L1, L2).

model_var(V,V) :- atom(V),  sub_atom(V, _, 1, 0, '_')
model_var(H,V):
  clause(H,B),
  runv

runiv(X,X) :- nonvar(X),!.
runiv(X0,X) :- X0 =.. L, member(X1,L), runiv(X1,X).
