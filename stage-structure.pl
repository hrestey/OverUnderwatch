:- [teams].

matchScore(Winner, Team1, Team2, MapScore).

match(matchScore(Winner, Team1, Team2, MapScore), Stage, Week).

unplayedMatch(Team1, Team2).

schedule(Matches).

record(Team, Wins, Losses, MapDifferential).

headToHeadMapDiff(Team1, Team2, HeadToHeadOwner).
headToHeadMapDiff(Team1, Team2, tie).

headToHead(Team1, Team2, Winner).
headToHead(Team1, Team2, tie).

tieBreakerMatch(Team1, Team2, Winner).

endOfStage(Wins, Losses). % checks if the Wins and Losses added together is evenly divisible by 10 (therefore end of stage)

compareRecords(>, Record1, Record2) :-
    record(team(Team1), W1, L1, MD1), record(team(Team2), W2, L2, MD2),
    W1 > W2.
compareRecords(>, Record1, Record2) :-
    record(team(Team1), W1, L1, MD1), record(team(Team2), W2, L2, MD2),
    W1 = W2,
    MD1 > MD2.
compareRecords(>, Record1, Record2) :-
    record(team(Team1), W1, L1, MD1), record(team(Team2), W2, L2, MD2),
    W1 = W2,
    MD1 = MD2,
    headToHeadMapDiff(Team1, Team2, Team1). % Head to Head Map Differential in favor of Team1
compareRecords(>, Record1, Record2) :-
    record(team(Team1), W1, L1, MD1), record(team(Team2), W2, L2, MD2),
    W1 = W2,
    MD1 = MD2,
    headToHeadMapDiff(Team1, Team2, tie),
    headToHead(Team1, Team2, Team1).
compareRecords(>, Record1, Record2) :- % should be restricted to only at the end of stages
    record(team(Team1), W1, L1, MD1), record(team(Team2), W2, L2, MD2),
    W1 = W2,
    MD1 = MD2,
    headToHead(MapDiff(Team1, Team2, tie),
    headToHead(Team1, Team2, tie),
    endOfStage(W1, L1),
    tieBreakerMatch(Team1, Team2, Team1).
compareRecords(=, Record1, Record2) :-
    record(team(Team1), W1, L1, MD1), record(team(Team2), W2, L2, MD2),
    W1 = W2,
    MD1 = MD2,
    headToHeadMadDiff(Team1, Team2, tie),
    headToHead(Team1, Team2, tie),
    /=endOfStage(W1, L1).
compareRecords(<, Team1, Team2) :-
    compareRecords(>, Team2, Team1).

teamStandings([], [_]).
teamStandings(Records, Standings, FinalStandings) :-
    select(Record, Records, RemainingRecords),
    compareRecords(Record, [BestRecord|RemainingStandings], BetterRecord), % if the best record is better than the Record being compared to it, call teamStandings on the Record and Remaining records
    teamStandings(Record, RemainingStandings, UpdatedStandings).
teamStandings(Records, [], Standings):-
    select(Record, Records, RemainingRecords),
    teamStandings(RemainingRecords, [Record], Standings).

