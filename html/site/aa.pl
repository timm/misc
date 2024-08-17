:- use_module(library(ugraphs)).

c2l((-X,Y),[-X|T]) :- !, c2l(Y,T).
c2l(-X,[-X]) :- atom(X).

graphs(G) :- bagof(X,graph(X),G).

graph(Head-[]):- clause(Head,true).
graph(Head-List):-
  clause(Head,Body), c2l(Body,List).

go :-
  G= [top-[f,a,b],
a -  [ c,d],
b - [e,g],
c - [],
d - [],
e - [],
g - [],
f-[z, k,l],
z-[],
k-[],
l-[]
],
top_sort(G,G1), print(G1).
