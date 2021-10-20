go :- tell('circ.data'), go1,told.

go1 :-
	setof(circuit2(Sw1,Sw2,Sw3,B1,B2,B3,L1,L2,L3),
              circuit2(Sw1,Sw2,Sw3,B1,B2,B3,L1,L2,L3),
	      L),
	forall(member(X,L),c45(X)).

c45(circuit2(Sw1,Sw2,Sw3,ok,_,_,L1,L2,L3)) :-
	format('~a, ~a, ~a, ~a, ~a, ~a, b1ok~n',
               [Sw1,Sw2,Sw3,L1,L2,L3]).
c45(circuit2(Sw1,Sw2,Sw3,blown,_,_,L1,L2,L3)) :-
	format('~a, ~a, ~a, ~a, ~a, ~a, b1blown~n',
               [Sw1,Sw2,Sw3,L1,L2,L3]).

eg(circuit2(Sw1,Sw2,Sw3,B1,B2,B3,L1,L2,L3)) :-
  circuit2(Sw1,Sw2,Sw3,B1,B2,B3,L1,L2,L3).

circuit2(Sw1,Sw2,Sw3,B1,B2,B3,L1,L2,L3) :-
	VSw3 = VB3,
	sum(VSw1, VB1, V1),
	sum(V1,VB3,+),
	sum(VSw2,VB2,VB3),
	switch(Sw1,VSw1,C1),
	bulb(B1,L1,VB1,C1),
	switch(Sw2,VSw2,C2),
	bulb(B2,L2,VB2,C2),
	switch(Sw3,VSw3,CSw3),
	bulb(B3,L3,VB3,CB3),
	sum(CSw3,CB3,C3),
	sum(C2,C3,C1).

switch(on,0,_).
switch(off,_,0).

bulb(blown,dark,_,0).
bulb(ok,light,+,+).
bulb(ok,light,-,-).
bulb(ok,dark,0,0).

sum(+,+,+). % %%%%
sum(+,0,+).
sum(+,-,_).
sum(0,+,+).
sum(0,0,0).
sum(0,-,-).
sum(-,+,_).
sum(-,0,-).
sum(-,-,-).