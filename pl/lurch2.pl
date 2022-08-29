% depth first iterative deepening
prove(Goal,Dmax,How) :- between(1,DMax, D),  prove1(Goal,D, [], How).

prove1(X,Y,How0,How) :- once(prove2(X,Y,How0,How)). % look ma, no cuts

prove2(true,_,How,How).
prove2((Goal,Goals),D) -->   prove1(Goal,D), prove1(Goals,D).
prove2(Goal, D, How0,How) :- 
       D  >0
       randomlySelect(Goal, How0, SubGoal, Id),  
       prove1(SubGoal, D-1, [Id|How0],How]).

randomlySelect(X, How, Z, Zid) :- 
        setof(R/X/Id,  (clause(X,Y,Id), randomlySelect1(Id,How,R)),   [_/Z/Zid|_])

% here where wou bias the choice of sub-goals.
% insert whole phd here. maybe just build a bayes classifier. collect clause ids
% (the "How"s) for each proof and sort them into best or rest. when you arrive here via some
% set of ids, select the subclause which, along with the path of ids coming in,
% maximizes likelihood of best
randomlySelect1(_,_,R) :- random(0.0, 1, R).

