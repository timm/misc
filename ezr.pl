term_expansion(F=Y, Out) :-
  fields(Y, Fields, Defaults),
  findall(Z, slot1(F, Fields, Defaults, Z), Out).

fields([],      [],     []).
fields([F=D|L], [F|Fs], [D|Ds]) :- !, fields(L,Fs,Ds).
fields([F  |L], [F|Fs], [_|Ds]) :-    fields(L,Fs,Ds).

slot1(F, _, Defaults, new(F,T)) :- T =.. [F|Defaults].
slot1(F, Fields, _, slot(F,Field,Old,New,T0,T)) :- 
  member(Field,Fields),
  slot2(Field, Old, New, Fields, L0, L),
  T0 =.. [F|L0],
  T  =.. [F|L ].

slot2(_,_,_,[],    [],    []).
slot2(F,X,Y,[F|FS],[X|L0],[Y|L]) :- !, slot2(F,X,Y,FS,L0,L).
slot2(F,X,Y,[_|FS],[Z|L0],[Z|L]) :-    slot2(F,X,Y,FS,L0,L).

