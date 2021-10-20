% vim: set filetype=prolog: ts=2 sw=2 sts=2 expandtab:cindent:formatoptions+=cro %

% defining grand dad
grandfather(X,Y) :- parent(X,Z),father(Z,Y).

parent(X,Y) :- mother(X,Y).
parent(X,Y) :- father(X,Y).

father(arthur,buck).
father(al,buck).
father(amber,buck).
father(anne,boris).
father(barbara,charles).
father(betty,cuthbert).
father(boris,charles).
father(buck,calvin).
father(charles,burt).

mother(al,barbara).
mother(anne,betty).
mother(arthur,barbara).
mother(amber,barbara).
mother(boris,carla).
mother(barbara,carla).
mother(betty,cora).