:- [teams].

match(Winner, Team1, Team2, MapScore).

unplayedMatch(Team1, Team2).

schedule(Matches).

record(Team, Wins, Losses, MapDifferential).

headToHead(Team1, Team2, Winner).

compareRecords(Record1, Record2, BetterRecord) :-
    .

teamStandings([], [_]).
teamStandings(Records, Standings, FinalStandings) :-
    select(Record, Records, RemainingRecords),
    .
teamStandings(Records, [], Standings):-
    select(Record, Records, RemainingRecords),
    teamStandings(RemainingRecords, [Record], Standings).


