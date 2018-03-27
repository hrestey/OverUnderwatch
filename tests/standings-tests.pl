:- begin_tests('../src/prolog/stage-structure').
:- ['../src/prolog/stage-structure'].

% test endOfStage, make sure it evaluates as true when wins and losses add to 10
test(endOfStage) :-
    endOfStage(7, 3).
test(endOfStage) :-
    \+endOfStage(8, 0).

% test teamStandings
test(teamStandings) :- 
    teamStandings([record(team(newyork), 10, 0, 25),
    record(team(london), 9, 1, 24),
    record(team(houston), 8, 2, 23),
    record(team(boston), 5, 5, 0),
    record(team(dallas), 6, 4, 10),
    record(team(lavaliant), 2, 8, -20),
    record(team(seoul), 7, 3, 10),
    record(team(lagladiators), 4, 6, -2),
    record(team(sanfrancisco), 3, 7, -5),
    record(team(philadelphia), 6, 4, -2),
    record(team(shanghai), 0, 10, -25),
    record(team(florida), 1, 9, -15)],
    [(1, [team(newyork)]),
    (2, [team(london)]),
    (3, [team(houston)]),
    (4, [team(seoul)]),
    (5, [team(dallas)]), 
    (6, [team(philadelphia)]),
    (7, [team(boston)]), 
    (8, [team(lagladiators)]), 
    (9, [team(sanfrancisco)]), 
    (10, [team(lavaliant)]), 
    (11, [team(florida)]),
    (12, [team(shanghai)])]).
test(teamStandings) :-
    teamStandings([record(team(dallas), 3, 5, -11),
    record(team(newyork), 8, 0, 21),
    record(team(shanghai), 0, 8, -30),
    record(team(philadelphia), 6, 2, -4),
    record(team(houston), 7, 1, 15),
    record(team(seoul), 7, 1, 9),
    record(team(lavaliant), 7, 1, 13),
    record(team(lagladiators), 4, 4, -10),
    record(team(sanfrancisco), 3, 5, -8),
    record(team(florida), 1, 7, -22),
    record(team(boston), 6, 2, 10),
    record(team(london), 7, 1, 15)],
    [(1,[team(newyork)]),
    (2,[team(houston),team(london)]),
    (3,[team(lavaliant)]),
    (4,[team(seoul)]),
    (5,[team(boston)]),
    (6,[team(philadelphia)]),
    (7,[team(lagladiators)]),
    (8,[team(sanfrancisco)]),
    (9,[team(dallas)]),
    (10,[team(florida)]),
    (11,[team(shanghai)])]).
test(teamStandings) :- 
    teamStandings([record(team(seoul), 6, 0, 14),
    record(team(newyork), 5, 1, 15),
    record(team(lagladiators), 4, 2, 10),
    record(team(london), 4, 2, 8),
    record(team(lavaliant), 3, 3, 3),
    record(team(philadelphia), 3, 3, 1),
    record(team(houston), 3, 3, -3),
    record(team(dallas), 2, 4, -4),
    record(team(sanfrancisco), 2, 4, -5),
    record(team(boston), 2, 4, -8),
    record(team(florida), 1, 5, -12),
    record(team(shanghai), 0, 6, -19)],
    [(1, [team(seoul)]),
    (2, [team(newyork)]),
    (3, [team(lagladiators)]),
    (4, [team(london)]),
    (5, [team(lavaliant)]),
    (6, [team(philadelphia)]),
    (7, [team(houston)]),
    (8, [team(dallas)]),
    (9, [team(sanfrancisco)]),
    (10, [team(boston)]),
    (11, [team(florida)]),
    (12, [team(shanghai)])]).

:- end_tests('../src/prolog/stage-structure').
