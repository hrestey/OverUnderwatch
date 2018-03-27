:- begin_tests('../src/prolog/stage-structure').
:- ['../src/prolog/stage-structure'].
:- ['tied-teams'].
% Testing a tie for Team Standings
test(teamStandings) :-
    teamStandings([record(team(sanfrancisco), 0, 0, 0),
    record(team(philadelphia), 0, 0, 0),
    record(team(houston), 0, 0, 0),
    record(team(lavaliant), 0, 0, 0),
    record(team(lagladiators), 0, 0, 0),
    record(team(shanghai), 0, 0, 0),
    record(team(boston), 0, 0, 0),
    record(team(london), 0, 0, 0),
    record(team(florida), 0, 0, 0),
    record(team(dallas), 0, 0, 0),
    record(team(seoul), 0, 0, 0),
    record(team(newyork), 0, 0, 0)],
    [(1, [team(boston),team(dallas),team(florida),team(houston),team(lagladiators),team(lavaliant),team(london),team(newyork),team(philadelphia),team(sanfrancisco),team(seoul),team(shanghai)])]).
test(teamStandings) :-
    teamStandings([record(team(newyork), 2, 0, 6),
    record(team(london), 2, 0, 6),
    record(team(seoul), 2, 0, 6),
    record(team(boston), 2, 0, 4),
    record(team(houston), 1, 1, 3),
    record(team(shanghai), 0, 2, -5),
    record(team(lagladiators), 1, 1, 3),
    record(team(lavaliant), 1, 1, 3),
    record(team(sanfrancisco), 0, 2, -5),
    record(team(philadelphia), 0, 2, -3),
    record(team(florida), 0, 2, -4),
    record(team(dallas), 0, 2, -1)],
    [(1, [team(london),team(newyork),team(seoul)]), (2, [team(boston)]), (3, [team(houston),team(lagladiators),team(lavaliant)]), (4, [team(dallas)]), 
    (5, [team(philadelphia)]), (6, [team(florida)]), (7, [team(sanfrancisco),team(shanghai)])]).


:- end_tests('../src/prolog/stage-structure').
