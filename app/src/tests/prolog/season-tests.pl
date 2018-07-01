:- begin_tests('../../prolog/stage-structure').
:- ['../../prolog/stage-structure').

% Tests for aFullOWLSeason

test(aFullOWLSeason) :-
    aFullOWLSeason([record(team(sanfrancisco), 0, 0, 0, [[london, 0], [houston, 0], [dallas, 0], [newyork, 0], [philadelphia, 0], [lavaliant, 0],
    [lagladiators, 0], [seoul, 0], [shanghai, 0], [boston, 0], [florida, 0]], [[london, 0], [houston, 0], [dallas, 0], [newyork, 0], [philadelphia, 0], [lavaliant, 0],
    [lagladiators, 0], [seoul, 0], [shanghai, 0], [boston, 0], [florida, 0]], [none]),
    record(team(philadelphia), 0, 0, 0, [[london, 0], [houston, 0], [dallas, 0], [newyork, 0], [sanfrancisco, 0], [lavaliant, 0], [lagladiators, 0],
    [seoul, 0], [shanghai, 0], [boston, 0], [florida, 0]], [[london, 0], [houston, 0], [dallas, 0], [newyork, 0], [sanfrancisco, 0], [lavaliant, 0],
    [lagladiators, 0], [seoul, 0], [shanghai, 0], [boston, 0], [florida, 0]], [none]),
    record(team(houston), 0, 0, 0, [[london, 0], [philadelphia, 0], [dallas, 0], [newyork, 0], [sanfrancisco, 0], [lavaliant, 0], [lagladiators, 0],
    [seoul, 0], [shanghai, 0], [boston, 0], [florida, 0]], [[london, 0], [philadelphia, 0], [dallas, 0], [newyork, 0], [sanfrancisco, 0], [lavaliant, 0],
    [lagladiators, 0], [seoul, 0], [shanghai, 0], [boston, 0], [florida, 0]], [none]),
    record(team(lavaliant), 0, 0, 0, [[london, 0], [philadelphia, 0], [dallas, 0], [newyork, 0], [sanfrancisco, 0], [houston, 0], [lagladiators, 0],
    [seoul, 0], [shanghai, 0], [boston, 0], [florida, 0]], [[london, 0], [philadelphia, 0], [dallas, 0], [newyork, 0], [sanfrancisco, 0], [houston, 0],
    [lagladiators, 0], [seoul, 0], [shanghai, 0], [boston, 0], [florida, 0]], [none]),
    record(team(lagladiators), 0, 0, 0, [[london, 0], [philadelphia, 0], [dallas, 0], [newyork, 0], [sanfrancisco, 0], [houston, 0], [lavaliant, 0],
    [seoul, 0], [shanghai, 0], [boston, 0], [florida, 0]], [[london, 0], [philadelphia, 0], [dallas, 0], [newyork, 0], [sanfrancisco, 0], [houston, 0],
    [lavaliant, 0], [seoul, 0], [shanghai, 0], [boston, 0], [florida, 0]], [none]),
    record(team(shanghai), 0, 0, 0, [[london, 0], [philadelphia, 0], [dallas, 0], [newyork, 0], [sanfrancisco, 0], [houston, 0], [lavaliant, 0],
    [seoul, 0], [lagladiators, 0], [boston, 0], [florida, 0]], [[london, 0], [philadelphia, 0], [dallas, 0], [newyork, 0], [sanfrancisco, 0], [houston, 0],
    [lavaliant, 0], [seoul, 0], [lagladiators, 0], [boston, 0], [florida, 0]], [none]),
    record(team(boston), 0, 0, 0, [[london, 0], [philadelphia, 0], [dallas, 0], [newyork, 0], [sanfrancisco, 0], [houston, 0], [lavaliant, 0],
    [seoul, 0], [lagladiators, 0], [shanghai, 0], [florida, 0]], [[london, 0], [philadelphia, 0], [dallas, 0], [newyork, 0], [sanfrancisco, 0], [houston, 0],
    [lavaliant, 0], [seoul, 0], [lagladiators, 0], [shanghai, 0], [florida, 0]], [none]),
    record(team(london), 0, 0, 0, [[boston, 0], [philadelphia, 0], [dallas, 0], [newyork, 0], [sanfrancisco, 0], [houston, 0], [lavaliant, 0],
    [seoul, 0], [lagladiators, 0], [shanghai, 0], [florida, 0]], [[boston, 0], [philadelphia, 0], [dallas, 0], [newyork, 0], [sanfrancisco, 0], [houston, 0],
    [lavaliant, 0], [seoul, 0], [lagladiators, 0], [shanghai, 0], [florida, 0]], [none]),
    record(team(florida), 0, 0, 0, [[boston, 0], [philadelphia, 0], [dallas, 0], [newyork, 0], [sanfrancisco, 0], [houston, 0], [lavaliant, 0],
    [seoul, 0], [lagladiators, 0], [shanghai, 0], [london, 0]], [[boston, 0], [philadelphia, 0], [dallas, 0], [newyork, 0], [sanfrancisco, 0], [houston, 0],
    [lavaliant, 0], [seoul, 0], [lagladiators, 0], [shanghai, 0], [london, 0]], [none]),
    record(team(dallas), 0, 0, 0, [[boston, 0], [philadelphia, 0], [florida, 0], [newyork, 0], [sanfrancisco, 0], [houston, 0], [lavaliant, 0],
    [seoul, 0], [lagladiators, 0], [shanghai, 0], [london, 0]], [[boston, 0], [philadelphia, 0], [florida, 0], [newyork, 0], [sanfrancisco, 0], [houston, 0],
    [lavaliant, 0], [seoul, 0], [lagladiators, 0], [shanghai, 0], [london, 0]], [none]),
    record(team(seoul), 0, 0, 0, [[boston, 0], [philadelphia, 0], [florida, 0], [newyork, 0], [sanfrancisco, 0], [houston, 0], [lavaliant, 0],
    [dallas, 0], [lagladiators, 0], [shanghai, 0], [london, 0]], [[boston, 0], [philadelphia, 0], [florida, 0], [newyork, 0], [sanfrancisco, 0], [houston, 0],
    [lavaliant, 0], [dallas, 0], [lagladiators, 0], [shanghai, 0], [london, 0]], [none]),
    record(team(newyork), 0, 0, 0, [[boston, 0], [philadelphia, 0], [florida, 0], [seoul, 0], [sanfrancisco, 0], [houston, 0], [lavaliant, 0],
    [dallas, 0], [lagladiators, 0], [shanghai, 0], [london, 0]], [[boston, 0], [philadelphia, 0], [florida, 0], [seoul, 0], [sanfrancisco, 0], [houston, 0],
    [lavaliant, 0], [dallas, 0], [lagladiators, 0], [shanghai, 0], [london, 0]], [none])],
    [[[[team(sanfrancisco), 0, team(lavaliant), 4, false], [team(shanghai), 0, team(lagladiators), 4, false], [team(dallas), 1, team(seoul), 2, false], [team(london), 3, team(florida), 1, false],
    [team(philadelphia), 3, team(houston), 2], [team(boston), 1, team(newyork), 3, false], [team(lavaliant, 3, team(dallas), 0, false], [team(florida), 0, team(boston), 4, false],
    [team(sanfrancisco), 3, team(shanghai), 1, false], [team(london), 4, team(philadelphia), 0, false], [team(newyork), 3, team(houston), 1, false], [team(seoul), 4, team(lagladiators), 0, false]], 
    [[team(sanfrancisco), 1, team(philadelphia), 2, false], [team(florida), 0, team(seoul), 4, false], [team(houston), 4, team(shanghai), 0, false], [team(dallas), 0, team(houston), 4, false], 
    [team(newyork), 3, team(lavaliant), 0, false], [team(philadelphia), 2, team(lagladiators), 3, false], [team(seoul), 4, team(boston), 0, false], [team(shanghai), 0, team(florida), 4, false],
    [team(london), 3, team(dallas), 1, false], [team(lavaliant), 2, team(london), 3, false], [team(lagladiators), 0, team(newyork), 4, false], [team(boston), 2, team(sanfrancisco), 3, false]],
    [[team(sanfrancisco), 3, team(london), 1, false], [team(shanghai), 1, team(seoul), 3, false], [team(lagladiators), 
