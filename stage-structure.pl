:- [teams].

/*matchScore(Winner, Team1, Team2, MapScore).

match(matchScore(Winner, Team1, Team2, MapScore), Stage, Week).

unplayedMatch(Team1, Team2).

schedule(Matches).

record(team(TeamName), Wins, Losses, MapDifferential).
Examples:
record(team(dallas), 10, 20, 5).
record(team(seoul), 11, 5, 7).
*/
%headToHeadMapDiff(Team1, Team2, HeadToHeadOwner) :-
%    .
%headToHeadMapDiff(Team1, Team2, tie).

/*headToHead(Team1, Team2, Winner).
headToHead(Team1, Team2, tie).

tieBreakerMatch(Team1, Team2, Winner).*/

headToHeadMapDiff(dallas, seoul, seoul).
headToHeadMapDiff(houston, london, tie).
headToHeadMapDiff(florida, dallas, tie).
headToHeadMapDiff(X, Y, X) :- headToHeadMapDiff(Y, X, X), !.
headToHeadMapDiff(X, Y, tie) :- headToHeadMapDiff(Y, X, tie), !.

headToHead(dallas, florida, dallas).
headToHead(houston, london, tie).
headToHead(X, Y, X) :- headToHead(Y, X, X), !.
headToHead(X, Y, tie) :- headToHead(Y, X, tie), !.

endOfStage(Wins, Losses) :-
    Sum is Wins + Losses,
    Result is Sum mod 10,
    Result = 0. % checks if the Wins and Losses added together is evenly divisible by 10 (therefore end of stage)

compareRecords(>, Record1, Record2) :-
    Record1 = record(team(_), W1, _, _), 
    Record2 = record(team(_), W2, _, _),
    W1 > W2.
compareRecords(>, Record1, Record2) :-
    Record1 = record(team(_), W1, _, MD1),
    Record2 = record(team(_), W2, _, MD2),
    W1 = W2,
    MD1 > MD2.
compareRecords(>, Record1, Record2) :-
    Record1 = record(team(Team1), W1, _, MD1),
    Record2 = record(team(Team2), W2, _, MD2),
    W1 = W2,
    MD1 = MD2,
    headToHeadMapDiff(Team1, Team2, Team1). % Head to Head Map Differential in favor of Team1
compareRecords(>, Record1, Record2) :-
    Record1 = record(team(Team1), W1, _, MD1),
    Record2 = record(team(Team2), W2, _, MD2),
    W1 = W2,
    MD1 = MD2,
    headToHeadMapDiff(Team1, Team2, tie),
    headToHead(Team1, Team2, Team1).
compareRecords(>, Record1, Record2) :- % should be restricted to only at the end of stages
    Record1 = record(team(Team1), W1, L1, MD1),
    Record2 = record(team(Team2), W2, _, MD2),
    W1 = W2,
    MD1 = MD2,
    headToHeadMapDiff(Team1, Team2, tie),
    headToHead(Team1, Team2, tie),
    endOfStage(W1, L1),
    tieBreakerMatch(Team1, Team2, Team1).
compareRecords(=, Record1, Record2) :-
    Record1 = record(team(Team1), W1, L1, MD1),
    Record2 = record(team(Team2), W2, _, MD2),
    W1 = W2,
    MD1 = MD2,
    headToHeadMapDiff(Team1, Team2, tie),
    headToHead(Team1, Team2, tie),
    \+endOfStage(W1, L1).

compareRecords(<, Team1, Team2) :-
    compareRecords(>, Team2, Team1).

teamStandings(Records, Standings) :-
    predsort(compareRecords, Records, Standings).


