:- use_module(library(lists)).

% Calculate entropy of a list
entropy([], 0).
entropy(Values, Entropy) :-
    length(Values, N),
    maplist(log2, Values, LogValues),
    sum_list(Values, Total),
    maplist(divide(Total), Values, Probabilities),
    maplist(multiply(-1), Probabilities, WeightedLogValues),
    sum_list(WeightedLogValues, WeightedEntropy),
    Entropy is WeightedEntropy / N.

% Calculate log base 2 of a number
log2(0, 0).
log2(X, Log) :- Log is log(X) / log(2).

:- arithmetic_function(logn/2)
logn(X,N,Out) :- Out is log(X) / log(N).

:- arithmetic_function(len/1)
len(X,Out) :- length(X,N).

% Divide two numbers
divide(N, M, Result) :- Result is N / M.

% Multiply two numbers
multiply(N, M, Result) :- Result is N * M.

% Split the dataset into attribute values
split_examples([], _, [], []).
split_examples([[Class|T]], AttributeIndex, [[Class|T]], []) :-
    length(T, AttributeIndex).
split_examples([[X|T]], AttributeIndex, [[X|Rest]], [T|SplitTail]) :-
    nth1(AttributeIndex, T, AttributeValue),
    split_examples([[X|T]], AttributeIndex, Rest, SplitTail).

% Calculate information gain for a given attribute
information_gain(AttributeIndex, Examples, InformationGain) :-
    split_examples(Examples, AttributeIndex, WithAttribute, WithoutAttribute),
    entropy(WithAttribute, WithAttributeEntropy),
    entropy(WithoutAttribute, WithoutAttributeEntropy),
    length(Examples, Total),
    length(WithAttribute, WithAttributeCount),
    length(WithoutAttribute, WithoutAttributeCount),
    divide(WithAttributeCount, Total, WithAttributeRatio),
    divide(WithoutAttributeCount, Total, WithoutAttributeRatio),
    multiply(WithAttributeRatio, WithAttributeEntropy, WeightedWithAttributeEntropy),
    multiply(WithoutAttributeRatio, WithoutAttributeEntropy, WeightedWithoutAttributeEntropy),
    InformationGain is -WeightedWithAttributeEntropy - WeightedWithoutAttributeEntropy.

% Find the best attribute to split on
best_attribute(Examples, BestAttributeIndex) :-
    length(Examples, [FirstRow|_]),
    length(FirstRow, NumAttributes),
    findall(InformationGain-AttributeIndex, (
        between(1, NumAttributes, AttributeIndex),
        AttributeIndex \= 1,
        information_gain(AttributeIndex, Examples, InformationGain)
    ), InformationGains),
    max_member(_-BestAttributeIndex, InformationGains).

% Build decision tree
build_tree([], _-DefaultClass, DefaultClass).
build_tree(Examples, _-DefaultClass, DefaultClass) :-
    all_same_class(Examples).
build_tree(Examples, BestAttributeIndex-BestAttribute, DefaultClass) :-
    split_examples(Examples, BestAttributeIndex, WithAttribute, WithoutAttribute),
    best_attribute(WithAttribute, NextBestAttributeIndex),
    build_tree(WithAttribute, NextBestAttributeIndex-BestAttribute, DefaultClassWith),
    build_tree(WithoutAttribute, NextBestAttributeIndex-BestAttribute, DefaultClassWithout),
    build_tree(Examples, BestAttributeIndex, BestAttribute, NextBestAttributeIndex, DefaultClassWith, DefaultClassWithout).

% Check if all examples have the same class
all_same_class([]).
all_same_class([[Class|_]|T]) :-
    maplist(=(Class), T).

% Classify a new example using the decision tree
classify([Class|_], Class) :-
    atomic(Class).
classify([Attribute|Rest], Class) :-
    functor(Attribute, AttributeName, _),
    arg(1, Attribute, AttributeValue),
    (  AttributeName = BestAttribute,
       build_tree(Examples, _-BestAttribute, DefaultClass),
       (  member([AttributeValue|Rest], Examples),
          classify([AttributeValue|Rest], Class)
       ;  classify(Rest, Class)
       )
    ;  classify(Rest, Class)
    ).

% Example usage:
% discretize([[outlook,temperature,humidity,wind,play],
%             [sunny,hot,high,weak,no],
%             [sunny,hot,high,strong,no],
%             [overcast,hot,high,weak,yes],
%             [rain,mild,high,weak,yes],
%             [rain,cool,normal,weak,yes],
%             [rain,cool,normal,strong,no],
%             [overcast,cool,normal,strong,yes],
%             [sunny,mild,high,weak,no],
%             [sunny,cool,normal,weak,yes],
%             [rain,mild,normal,weak,yes],
%             [sunny,mild,normal,strong,yes],
%             [overcast,mild,high,strong,yes],
%             [overcast,hot,normal,weak,yes],
%             [rain,mild,high,strong,no]],
%            [sunny, hot, high, strong], Result).

discretize(Examples, NewExample, Result) :-
    best_attribute(Examples, BestAttributeIndex),
    split_examples(Examples, BestAttributeIndex, WithAttribute, WithoutAttribute),
    build_tree(Examples, BestAttributeIndex-BestAttributeIndex, DefaultClass),
    classify(NewExample, Result).

discretize([[outlook,temperature,humidity,wind,play],
             [sunny,hot,high,weak,no],
             [sunny,hot,high,strong,no],
             [overcast,hot,high,weak,yes],
             [rain,mild,high,weak,yes],
             [rain,cool,normal,weak,yes],
             [rain,cool,normal,strong,no],
             [overcast,cool,normal,strong,yes],
             [sunny,mild,high,weak,no],
             [sunny,cool,normal,weak,yes],
             [rain,mild,normal,weak,yes],
             [sunny,mild,normal,strong,yes],
             [overcast,mild,high,strong,yes],
             [overcast,hot,normal,weak,yes],
             [rain,mild,high,strong,no]],
            [sunny, hot, high, strong], Result).

