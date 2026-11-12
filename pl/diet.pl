/*  diet.pl -- daily menu planner for diabetic students on a budget.

    Data: menus/<venue>.pl, one file per venue (20 Raleigh spots on or
    near Hillsborough St). Facts, sorted least to most (price, sugar):

        venue(Venue, "Pretty Name").
        item(Venue, Item, Course, PriceUSD, SugarGrams, Calories).

    A day = breakfast + lunch + dinner, each at one venue.
    Breakfast and lunch = starter + main. Dinner = starter + main + dessert.
    Hard limits per day: budget/1 (USD), sugar_cap/1 (grams),
    calorie target from profile/2 (USDA 2020-2025, age ~25,
    moderately active: male ~2700 kcal, female ~2100 kcal).
    Ranking: least cost first (cost matters most), then least sugar.

    Run:  swipl -q diet.pl            % prints top 5 plans
          ?- top(10).                 % or ask for more
*/

:- initialization(main).

budget(40.00).              % daily eating-out budget, USD
sugar_cap(60).              % daily added-sugar ceiling, grams
profile(male_25,   2700).   % kcal/day, moderately active
profile(female_25, 2100).
whoami(male_25).            % change to female_25 as needed

:- dynamic item/6, venue/2.
:- table best_meal/6.

load_menus :-
    expand_file_name('menus/*.pl', Files),
    Files \== [],
    forall(member(F, Files), load_facts(F)).

% read each file as data (consult would treat every file as a
% redefinition of item/6 and keep only the last one).
load_facts(F) :-
    setup_call_cleanup(
        open(F, read, S),
        ( repeat,
          read_term(S, T, []),
          ( T == end_of_file -> ! ; assertz(T), fail )
        ),
        close(S)).

%% meal(?Slot, ?Venue, -Items, -Cost, -Sugar, -Cal)
%  one venue per meal; breakfast/lunch = starter+main, dinner adds dessert.
meal(Slot, V, [S-main-M], C, G, K) :-
    member(Slot, [breakfast, lunch]),
    item(V, S, starter, C1, G1, K1),
    item(V, M, main,    C2, G2, K2),
    C is C1 + C2, G is G1 + G2, K is K1 + K2.
meal(dinner, V, [S-main-M-dessert-D], C, G, K) :-
    item(V, S, starter, C1, G1, K1),
    item(V, M, main,    C2, G2, K2),
    item(V, D, dessert, C3, G3, K3),
    C is C1 + C2 + C3, G is G1 + G2 + G3, K is K1 + K2 + K3.

%% best meal per venue per slot (least cost, then sugar) -- prunes search.
%  once+order_by = findall + sort + head, and fails on no solution.
%  (aggregate_all(min(...)) will not do: it is arithmetic-only.)
best_meal(Slot, V, Items, C, G, K) :-
    venue(V, _),
    once(order_by([asc(C), asc(G)], meal(Slot, V, Items, C, G, K))).

%% plan(-Plan, -Cost, -Sugar, -Cal): three meals, three different venues,
%  within budget, sugar cap, and the profile's calorie target.
plan(plan(b(BV,BI,BC,BG,BK), l(LV,LI,LC,LG,LK), d(DV,DI,DC,DG,DK)),
     Cost, Sugar, Cal) :-
    best_meal(breakfast, BV, BI, BC, BG, BK),
    best_meal(lunch,     LV, LI, LC, LG, LK),
    BV @< LV,   % breakfast and lunch have the same shape, so a
                % swapped pair is the same plan; keep one ordering
    best_meal(dinner,    DV, DI, DC, DG, DK),
    DV \== BV, DV \== LV,
    Cost is BC + LC + DC,
    budget(B),         Cost  =< B,
    Sugar is BG + LG + DG,
    sugar_cap(S),      Sugar =< S,
    Cal is BK + LK + DK,
    whoami(Who), profile(Who, Target),
    Cal =< Target.

top(N) :-
    findall(s(C,G,K,P), plan(P, C, G, K), Ps),
    sort(Ps, Sorted),                       % ascending cost, then sugar
    length(Ps, Total),
    budget(B), sugar_cap(Cap), whoami(Who), profile(Who, Target),
    format("~w plans fit budget $~2f, sugar cap ~wg, ~w kcal (~w). Top ~w:~n~n",
           [Total, B, Cap, Target, Who, N]),
    forall(limit(N, member(s(C,G,K,P), Sorted)), show(P, C, G, K)).

show(plan(B, L, D), C, G, K) :-
    format("=== total $~2f, sugar ~wg, ~w kcal ===~n", [C, G, K]),
    line(breakfast, B), line(lunch, L), line(dinner, D), nl.

line(Slot, Meal) :-
    Meal =.. [_, V, Items, C, G, K],
    venue(V, Name),
    format("  ~w at ~s ($~2f, ~wg, ~wkcal): ", [Slot, Name, C, G, K]),
    say(Items), nl.

say([S-main-M])           :- format("~w, then ~w", [S, M]).
say([S-main-M-dessert-D]) :- format("~w, then ~w, then ~w", [S, M, D]).

main :- load_menus, top(5), halt.
main :- halt(1).
