tree(K,  V,  node(K,V,_,_)).
tree(K0, V0, node(K,V,_,_)).


isempty(nil) :- !.
isempty(tree(nil,nil,nil)).

bTTree(tree(_,nil,nil)).
bTTree(tree(N,Left,nil)) :- Left@=<N.
bTTree(tree(N,nil,Right)) :- N@<Right.
bTTree(tree(_,Left,Right)) :- bTTree(Left), bTTree(Right).
bTTree(tree(N,Left,Right)) :- Left@=<N, N@<Right.


