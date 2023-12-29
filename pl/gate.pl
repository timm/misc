:- csv_read_file('/Users/timm/gits/timm/lo/data/auto93.csv',_R).
:- current_prolog_flag(argv, Args), print(Args),nl.

cliafter(X,Y) :- current_prolog_flag(argv, L),append(_,[X1,Y|_],L), string_concat('-',X,X1).
coerce(X,Y) :- atom_number(X,Z) -> Y=Z ; atom_string(Y,X).

:- atom_chars('-a',[_|T]), print(T). 