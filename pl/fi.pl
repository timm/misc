% vim: filetype=prolog
% Calculate log base 2 of a number
:- use_module(library(lists)).
log2(0, 0).
log2(X, Log) :- Log is log(X) / log(2).

:- arithmetic_function(logn/2).
logn(X,N,Out) :- Out is log(X) / log(N).

:- arithmetic_function(len/1).
len(X,Out) :- length(X,Out).

% Divide two numbers
divide(N, M, Result) :- Result is N / M.

% Multiply two numbers
multiply(N, M, Result) :- Result is N * M.

:-op(700,xfx,:=).
=(At,V,T, T)  :- meta(_,At,V,V,T,T).
:=(At,V,T0,T) :- meta(_,At,_,V,T0,T).
>=(At,N)     --> =(At,N1), {N1 >= N}.
>( At,N)     --> =(At,N1), {N1 >  N}.
<( At,N)     --> =(At,N1), {N1 <  N}.
=<(At,N)     --> =(At,N1), {N1 =< N}.

neg1(>=, <).
neg1(>, =<).
neg1(=, \=).
neg(X,Y) :- neg1(X,Y);  neg1(Y,X).

isa(F,Y) :- isa(F,Y,Y).
isa(F,Y,Y) :- meta(F,_,_,_,Y,Y).

term_expansion((Head0 = Fields --> Body0),
               [(Head :- Body),(Head1 :- Head)|Access]) :-
  xpandAccessors(Head0,Fields,Eg,Access),
  dcg_translate_rule((Head0 --> tmp,Body0), (Head :- tmp(Eg,Eg),Body)),
  xpandShortCut(Head0,Head1).
 
xpandShortCut(Head0,Head) :-
  Head0=.. Parts, append(Tmp,[_,Out],Parts), append(Tmp,[Out],Parts1), Head=.. Parts1.

xpandAccessors(Head0,Fields,T0,[meta(F,At,V0,V,T0,T1)|Access]) :-
  functor(Head0,F,_),
  findall(Access1, so(F=Fields,Access1), [meta(F,At,V0,V,T0,T1)|Access]).

xpandAccessor(F=L,meta(F,X,V0,V1,Term0,Term1)) :-
  member(X,L), xpandAccessor(L,V0,V1,L0,L1,X), Term0=.. [F|L0], Term1=.. [F|L1].

xpandAccessor([],    _, _,[],      [],      _).
xpandAccessor([H|L],V0,V1,[V0|V0s],[V1|V1s],H) :-         xpandAccessor(L,_, _, V0s,V1s,H).
xpandAccessor([H|L],V0,V1,[V |V0s],[V |V1s],X) :- H \= X, xpandAccessor(L,V0,V1,V0s,V1s,X).

end(S,Last) :- sub_atom(S,_,1,0,Last).

num(At,S) = [at,txt,n,mu,mu2,w] --> 
  at=At, txt=S, n= 0, mu= 0, mu2= 0,  
  ({end(S,"-")} -> w= -1 ; w= 1).

eg1 :-
   num(1,"fred-",_,N), print(N).

% :- spy(num).
% :- trace(num).
% :- listing(num). 
% :- listing(meta).
%
:- eg1.
% makeCol(Names,Row) :-
%   nth1(Name, Names, At)
%   name(Name, [First|_]),
%   upper(First) -> row(at
% makeCols(Names,row(x(X), y(Y), all(A), names(Names)) :-
%   findall(Col, makeCol(Names,Col)), Cols),
%
% split([],_,[],[]).
% split([R|Rows],Want,[R|Yes],No):- at(Want,R),!,split1(Rows,Want,Yes,No).
% split([R|Rows],Want,Yes,[R|No]):- split1(Rows,Want,Yes,No).
%
% gain(Want,Rows,Gain) :-
%   split(Rows,Want,Yes,No),
%   len(Yes)
% % Split the dataset into attribute values
%
%   
% split_examples([], _, [], []).
% split_examples([[Class|T]], AttributeIndex, [[Class|T]], []) :-
%     length(T, AttributeIndex).
% split_examples([[X|T]], AttributeIndex, [[X|Rest]], [T|SplitTail]) :-
%     nth1(AttributeIndex, T, AttributeValue),
%     split_examples([[X|T]], AttributeIndex, Rest, SplitTail).
%
% % Calculate information gain for a given attribute
% information_gain(AttributeIndex, Examples, InformationGain) :-
%     split_examples(Examples, AttributeIndex, WithAttribute, WithoutAttribute),
%     entropy(WithAttribute, WithAttributeEntropy),
%     entropy(WithoutAttribute, WithoutAttributeEntropy),
%     length(Examples, Total),
%     length(WithAttribute, WithAttributeCount),
%     length(WithoutAttribute, WithoutAttributeCount),
%     divide(WithAttributeCount, Total, WithAttributeRatio),
%     divide(WithoutAttributeCount, Total, WithoutAttributeRatio),
%     multiply(WithAttributeRatio, WithAttributeEntropy, WeightedWithAttributeEntropy),
%     multiply(WithoutAttributeRatio, WithoutAttributeEntropy, WeightedWithoutAttributeEntropy),
%     InformationGain is -WeightedWithAttributeEntropy - WeightedWithoutAttributeEntropy.
%
% % Find the best attribute to split on
% best_attribute(Examples, BestAttributeIndex) :-
%     length(Examples, [FirstRow|_]),
%     length(FirstRow, NumAttributes),
%     findall(InformationGain-AttributeIndex, (
%         between(1, NumAttributes, AttributeIndex),
%         AttributeIndex \= 1,
%         information_gain(AttributeIndex, Examples, InformationGain)
%     ), InformationGains),
%     max_member(_-BestAttributeIndex, InformationGains).
%
% % Build decision tree
% build_tree([], _-DefaultClass, DefaultClass).
% build_tree(Examples, _-DefaultClass, DefaultClass) :-
%     all_same_class(Examples).
% build_tree(Examples, BestAttributeIndex-BestAttribute, DefaultClass) :-
%     split_examples(Examples, BestAttributeIndex, WithAttribute, WithoutAttribute),
%     best_attribute(WithAttribute, NextBestAttributeIndex),
%     build_tree(WithAttribute, NextBestAttributeIndex-BestAttribute, DefaultClassWith),
%     build_tree(WithoutAttribute, NextBestAttributeIndex-BestAttribute, DefaultClassWithout),
%     build_tree(Examples, BestAttributeIndex, BestAttribute, NextBestAttributeIndex, DefaultClassWith, DefaultClassWithout).
%
% % Check if all examples have the same class
% all_same_class([]).
% all_same_class([[Class|_]|T]) :-
%     maplist(=(Class), T).
%
% % Classify a new example using the decision tree
% classify([Class|_], Class) :-
%     atomic(Class).
% classify([Attribute|Rest], Class) :-
%     functor(Attribute, AttributeName, _),
%     arg(1, Attribute, AttributeValue),
%     (  AttributeName = BestAttribute,
%        build_tree(Examples, _-BestAttribute, DefaultClass),
%        (  member([AttributeValue|Rest], Examples),
%           classify([AttributeValue|Rest], Class)
%        ;  classify(Rest, Class)
%        )
%     ;  classify(Rest, Class)
%     ).
%
% % Example usage:
% % discretize([[outlook,temperature,humidity,wind,play],
% %             [sunny,hot,high,weak,no],
% %             [sunny,hot,high,strong,no],
% %             [overcast,hot,high,weak,yes],
% %             [rain,mild,high,weak,yes],
% %             [rain,cool,normal,weak,yes],
% %             [rain,cool,normal,strong,no],
% %             [overcast,cool,normal,strong,yes],
% %             [sunny,mild,high,weak,no],
% %             [sunny,cool,normal,weak,yes],
% %             [rain,mild,normal,weak,yes],
% %             [sunny,mild,normal,strong,yes],
% %             [overcast,mild,high,strong,yes],
% %             [overcast,hot,normal,weak,yes],
% %             [rain,mild,high,strong,no]],
% %            [sunny, hot, high, strong], Result).
%
% discretize(Examples, NewExample, Result) :-
%     best_attribute(Examples, BestAttributeIndex),
%     split_examples(Examples, BestAttributeIndex, WithAttribute, WithoutAttribute),
%     build_tree(Examples, BestAttributeIndex-BestAttributeIndex, DefaultClass),
%     classify(NewExample, Result).
%
% discretize([[outlook,temperature,humidity,wind,play],
%              [sunny,hot,high,weak,no],
%              [sunny,hot,high,strong,no],
%              [overcast,hot,high,weak,yes],
%              [rain,mild,high,weak,yes],
%              [rain,cool,normal,weak,yes],
%              [rain,cool,normal,strong,no],
%              [overcast,cool,normal,strong,yes],
%              [sunny,mild,high,weak,no],
%              [sunny,cool,normal,weak,yes],
%              [rain,mild,normal,weak,yes],
%              [sunny,mild,normal,strong,yes],
%              [overcast,mild,high,strong,yes],
%              [overcast,hot,normal,weak,yes],
%              [rain,mild,high,strong,no]],
%             [sunny, hot, high, strong], Result).
%
