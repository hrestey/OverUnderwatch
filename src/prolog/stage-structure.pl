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

% the two teams (first two values) must be alphabetically sorted
% if called out-of-order (alphabetically), flip
headToHeadMapDiff(X, Y, X) :- X @> Y, headToHeadMapDiff(Y, X, X).
headToHeadMapDiff(X, Y, tie) :- X @> Y, headToHeadMapDiff(Y, X, tie).

% the two teams (first two values) must be alphabetically sorted
% if called out-of-order (alphabetically), flip
headToHead(X, Y, X) :- X @> Y, headToHead(Y, X, X).
headToHead(X, Y, tie) :- X @> Y, headToHead(Y, X, tie).

endOfStage(Wins, Losses) :-
    Sum is Wins + Losses,
    Sum = 10. 

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
    tieBreakerMatchPlayed(Team1, Team2),
    tieBreakerMatch(Team1, Team2, Team1).
% tie case
compareRecords(>, Record1, Record2) :-
    Record1 = record(team(Team1), W1, L1, MD1),
    Record2 = record(team(Team2), W2, _, MD2),
    W1 = W2,
    MD1 = MD2,
    headToHeadMapDiff(Team1, Team2, tie),
    headToHead(Team1, Team2, tie),
    \+endOfStage(W1, L1),
    % ensure alphabetical order of team names
    Team1 @> Team2.

compareRecords(<, Team1, Team2) :-
    compareRecords(>, Team2, Team1).

% label teams as 1st, 2nd, etc.
assignStandings(_, [], []).
assignStandings(N, [H|T], [(N, H)|Rest]) :-
    N2 is N + 1,
    assignStandings(N2, T, Rest).

teamStandings(Records, Standings) :-
    predsort(compareRecords, Records, SortedRecords),
    % group teams by their records, so we can establish standings
    findall(Teams, bagof(Team, member(record(Team, _, _, _), SortedRecords), Teams), GroupedRecords),
    reverse(GroupedRecords, AscendingRecords),
    assignStandings(1, AscendingRecords, Standings), !.


