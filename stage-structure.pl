:- [teams].

match(Winner, Team1, Team2, MapScore).

unplayedMatch(Team1, Team2).

schedule(Matches).

record(Team, NumberofGamesPlayed, Wins, Losses, MapDifferential).

headToHeadMapDiff(Team1, Team2, HeadToHeadOwner).
headToHeadMapDiff(Team1, Team2, tie).

headToHead(Team1, Team2, Winner).
headToHead(Team1, Team2, tie).

tieBreakerMatch(Team1, Team2, Winner).

compareRecords(>, Record1, Record2) :-
    record(team(Team1), NoG1, W1, L1, MD1), record(team(Team2), NoG2, W2, L2, MD2),
    NoG1 = NoG2,
    W1 > W2.
compareRecords(>, Record1, Record2) :-
    record(team(Team1), NoG1, W1, L1, MD1), record(team(Team2), NoG2, W2, L2, MD2),
    NoG1 = NoG2,
    W1 = W2,
    MD1 > MD2.
compareRecords(>, Record1, Record2) :-
    record(team(Team1), NoG1, W1, L1, MD1), record(team(Team2), NoG2, W2, L2, MD2),
    NoG1 = NoG2,
    W1 = W2,
    MD1 = MD2,
    headToHeadMapDiff(Team1, Team2, Team1). % Head to Head Map Differential in favor of Team1
compareRecords(>, Record1, Record2) :-
    record(team(Team1), NoG1, W1, L1, MD1), record(team(Team2), NoG2, W2, L2, MD2),
    NoG1 = NoG2,
    W1 = W2,
    MD1 = MD2,
    headToHeadMapDiff(Team1, Team2, tie),
    headToHead(Team1, Team2, Team1).
compareRecords(>, Record1, Record2) :-
    record(team(Team1(, NoG1, W1, L1, MD1), record(team(Team2), NoG2, W2, L2, MD2),
    NoG1 = NoG2,
    W1 = W2,
    MD1 = MD2,
    headToHead(MapDiff(Team1, Team2, tie),
    headToHead(Team1, Team2, tie),
    tieBreakerMatch(Team1, Team2, Team1).
compareRecords(>, Record1, Record2) :-
    record(team(Team1), NoG1, W1, L1, MD1), record(team(Team2), NoG2, W2, L2, MD2),
    NoG1 \= NoG2,
    W1 > W2,
    L1 < L2.
compareRecords(>, Record1, Record2) :-
    record(team(Team1), NoG1, W1, L1, MD1), record(team(Team2), NoG2, W2, L2, MD2),
    NoG1 \= NoG2,
    W1 > W2,
    L1 = L2.
compareRecords(>, Record1, Record2) :-
    record(team(Team1), NoG1, W1, L1, MD1), record(team(Team2), NoG2, W2, L2, MD2),
    NoG1 \= NoG2,
    W1 = W2,
    L1 < L2.
compareRecords(>, Record1, Record2) :-
    record(team(Team1), NoG1, W1, L1, MD1), record(team(Team2), NoG2, W2, L2, MD2),
    NoG1 \= NoG2,
    W1 = W2, 

teamStandings([], [_]).
teamStandings(Records, Standings, FinalStandings) :-
    select(Record, Records, RemainingRecords),
    compareRecords(Record, [BestRecord|RemainingStandings], BetterRecord), % if the best record is better than the Record being compared to it, call teamStandings on the Record and Remaining records
    teamStandings(Record, RemainingStandings, UpdatedStandings).
teamStandings(Records, [], Standings):-
    select(Record, Records, RemainingRecords),
    teamStandings(RemainingRecords, [Record], Standings).

