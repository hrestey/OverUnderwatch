:- [teams].

match(Winner, Team1, Team2, MapScore).

unplayedMatch(Team1, Team2).

schedule(Matches).

record(Record, Team, NumberofGamesPlayed, Wins, Losses, MapDifferential).

headToHead(Team1, Team2, Winner).

compareRecords(Record1, Record2, BetterRecord) :-
    record(Record1, team(Team1), NoG1, W1, L1, MD1), record(Record2, team(Team2), NoG2, W2, L2, MD2),
    .

teamStandings([], [_]).
teamStandings(Records, Standings, FinalStandings) :-
    select(Record, Records, RemainingRecords),
    .
teamStandings(Records, [], Standings):-
    select(Record, Records, RemainingRecords),
    teamStandings(RemainingRecords, [Record], Standings).


