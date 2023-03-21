% vim: set filetype=prolog :
the([file='../../tested/etc/data/auto93.csv']).

%is(X,Y) :- the(L), once(member(X=Y,L)).

:- discontiguous slot/6, def/2.
term_expansion(def(T), L) :- 
  findall(X, xpand(def1(T),X), L).

xpand(def1(Term), slot(Functor,Slot,Pos,Val1,Val2,T1,T2)) :-
  Term =.. [Functor|Slots],
  length(Slots,Arity),
  length(Vs1, Arity),
  length(Vs2, Arity),
  T1 =.. [Functor | Vs1],
  T2 =.. [Functor | Vs2],
  xpand1(Vs1, Vs2, Slots, Slot, Val1, Val2,1,Pos).

xpand1([X|Vs],[Y|Vs],[S|_], S,X,Y,Pos,Pos).
xpand1([V|Xs],[V|Ys],[_|Ss],S,X,Y,Pos0,Pos) :- 
  Pos1 is Pos0 + 1,
  xpand1(Xs,Ys,Ss,S, X,Y,Pos1,Pos).

%=(Functor/Slot=V)  -->  slot(Functor,Slot,_,_,V).
%==(Functor/Slot=V) -->  slot(Functor,Slot,_,V,V).
%
%on(Slot,V,L) :- member(Slot=V,L).
%on(Slot,V,W,L) :- on(Slot,V,W,L0,L).
%on(Slot,V,W,[Slot=V|T],[Slot=W|T]).
%on(Slot,V,W,[H|T0],[H|T]) :- on(Slot,V,W,T0,T).
%
num0(Num)  :- num0(0,"",Num).
num0(N,[H|S],num(N,[H|S],-1,[],fail,W)) :-
  string_chars(S,L), last(L,(-)).

def(num(at,txt,n,mu,m2,sd)).

:- listing(slot).

%go(F) :-
%  csv_read_file(F,[Names|Rows]),
%
%cols(Names,Cols) :- cols1([names=[],x=[],y=[],all=[]], Cols) 
%cols([],_,[]).
%
%upper1(S) :- string_chars(S,[H|_]), char_type(H,upper).

:- quit.
