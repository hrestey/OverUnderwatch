:- begin_tests('../src/prolog/stage-structure').
:- ['../src/prolog/stage-structure'].

% test endOfStage, make sure it evaluates as true when wins and losses add to 10
test(endOfStage) :-
    endOfStage(7, 3).
test(endOfStage) :-
    \+endOfStage(8, 0).

% test teamStandings
test(teamStandings) :-
    teamStandings([record(team(newyork), 10, 0, 25, [none], [none]),
    record(team(london), 9, 1, 24, [none], [none]),
    record(team(houston), 8, 2, 23, [none], [none]),
    record(team(boston), 5, 5, 0, [none], [none]),
    record(team(dallas), 6, 4, 10,[none], [none] ),
    record(team(lavaliant), 2, 8, -20, [none], [none]),
    record(team(seoul), 7, 3, 10, [none], [none]),
    record(team(lagladiators), 4, 6, -2, [none], [none]),
    record(team(sanfrancisco), 3, 7, -5, [none], [none]),
    record(team(philadelphia), 6, 4, -2, [none], [none]),
    record(team(shanghai), 0, 10, -25, [none], [none]),
    record(team(florida), 1, 9, -15, [none], [none])],
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
    teamStandings([record(team(dallas), 3, 5, -11, [none], [none]),
    record(team(newyork), 8, 0, 21, [none], [none]),
    record(team(shanghai), 0, 8, -30, [none], [none]),
    record(team(philadelphia), 6, 2, -4, [none], [none]),
    record(team(houston), 7, 1, 15, [none], [none]),
    record(team(seoul), 7, 1, 9, [none], [none]),
    record(team(lavaliant), 7, 1, 13, [none], [none]),
    record(team(lagladiators), 4, 4, -10, [none], [none]),
    record(team(sanfrancisco), 3, 5, -8, [none], [none]),
    record(team(florida), 1, 7, -22, [none], [none]),
    record(team(boston), 6, 2, 10, [none], [none]),
    record(team(london), 7, 1, 15, [none], [none])],
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
    teamStandings([record(team(seoul), 6, 0, 14, [none], [none]),
    record(team(newyork), 5, 1, 15, [none], [none]),
    record(team(lagladiators), 4, 2, 10, [none], [none]),
    record(team(london), 4, 2, 8, [none], [none]),
    record(team(lavaliant), 3, 3, 3, [none], [none]),
    record(team(philadelphia), 3, 3, 1, [none], [none]),
    record(team(houston), 3, 3, -3, [none], [none]),
    record(team(dallas), 2, 4, -4, [none], [none]),
    record(team(sanfrancisco), 2, 4, -5, [none], [none]),
    record(team(boston), 2, 4, -8, [none], [none]),
    record(team(florida), 1, 5, -12, [none], [none]),
    record(team(shanghai), 0, 6, -19, [none], [none])],
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
    % Testing a tie for Team Standings
test(teamStandings) :-
    teamStandings([record(team(sanfrancisco), 0, 0, 0, [none], [none]),
    record(team(philadelphia), 0, 0, 0, [none], [none]),
    record(team(houston), 0, 0, 0, [none], [none]),
    record(team(lavaliant), 0, 0, 0, [none], [none]),
    record(team(lagladiators), 0, 0, 0, [none], [none]),
    record(team(shanghai), 0, 0, 0, [none], [none]),
    record(team(boston), 0, 0, 0, [none], [none]),
    record(team(london), 0, 0, 0, [none], [none]),
    record(team(florida), 0, 0, 0, [none], [none]),
    record(team(dallas), 0, 0, 0, [none], [none]),
    record(team(seoul), 0, 0, 0, [none], [none]),
    record(team(newyork), 0, 0, 0, [none], [none])],
    [(1, [team(boston),team(dallas),team(florida),team(houston),team(lagladiators),team(lavaliant),team(london),team(newyork),team(philadelphia),team(sanfrancisco),team(seoul),team(shanghai)])]).
test(teamStandings) :-
    teamStandings([record(team(newyork), 2, 0, 6, [none], [none]),
    record(team(london), 2, 0, 6, [none], [none]),
    record(team(seoul), 2, 0, 6, [none], [none]),
    record(team(boston), 2, 0, 4, [none], [none]),
    record(team(houston), 1, 1, 3, [none], [none]),
    record(team(shanghai), 0, 2, -5, [none], [none]),
    record(team(lagladiators), 1, 1, 3, [none], [none]),
    record(team(lavaliant), 1, 1, 3, [none], [none]),
    record(team(sanfrancisco), 0, 2, -5, [none], [none]),
    record(team(philadelphia), 0, 2, -3, [none], [none]),
    record(team(florida), 0, 2, -4, [none], [none]),
    record(team(dallas), 0, 2, -1, [none], [none])],
    [(1, [team(london),team(newyork),team(seoul)]), (2, [team(boston)]), (3, [team(houston),team(lagladiators),team(lavaliant)]), (4, [team(dallas)]),
    (5, [team(philadelphia)]), (6, [team(florida)]), (7, [team(sanfrancisco),team(shanghai)])]).

:- end_tests('../src/prolog/stage-structure').
