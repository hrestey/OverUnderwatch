:- [teams].

/*matchScore(Winner, Team1, Team2, MapScore).

match(matchScore(Winner, Team1, Team2, MapScore), Stage, Week).

unplayedMatch(Team1, Team2).

schedule(Matches).

record(team(TeamName), Wins, Losses, MapDifferential).
Examples:
record(team(dallas), 10, 20, 5).
record(team(seoul), 11, 5, 7).


% the two teams (first two values) must be alphabetically sorted
% if called out-of-order (alphabetically), flip
headToHeadMapDiff(X, Y, X) :- X @> Y, headToHeadMapDiff(Y, X, X).
headToHeadMapDiff(X, Y, tie) :- X @> Y, headToHeadMapDiff(Y, X, tie).

% the two teams (first two values) must be alphabetically sorted
% if called out-of-order (alphabetically), flip
headToHead(X, Y, X) :- X @> Y, headToHead(Y, X, X).
headToHead(X, Y, tie) :- X @> Y, headToHead(Y, X, tie).*/

endOfStage(Wins, Losses) :-
    Sum is Wins + Losses,
    Sum = 10. 

%record(team(name), Wins, Losses, OverallMapDiff, HeadToHeadMapDiff, HeadToHead) -> HeadToHeadMapDiff and HeadToHead have team names in them if the team has a positive record against that team

compareRecords(>, Record1, Record2) :-
    Record1 = record(team(_), W1, _, _, [_], [_]), 
    Record2 = record(team(_), W2, _, _, [_], [_]),
    W1 > W2.
compareRecords(>, Record1, Record2) :-
    Record1 = record(team(_), W1, _, MD1, [_], [_]),
    Record2 = record(team(_), W2, _, MD2, [_], [_]),
    W1 = W2,
    MD1 > MD2.
compareRecords(>, Record1, Record2) :-
    Record1 = record(team(_), W1, _, MD1, HtHMD1, [_]),
    Record2 = record(team(Team2), W2, _, MD2, [_],[_]),
    W1 = W2,
    MD1 = MD2,
    member(Team2, HtHMD1). % Head to Head Map Differential in favor of Team1
compareRecords(>, Record1, Record2) :-
    Record1 = record(team(Team1), W1, _, MD1, HtHMD1, HtHR1),
    Record2 = record(team(Team2), W2, _, MD2, HtHMD2, [_]),
    W1 = W2,
    MD1 = MD2,
    \+member(Team2, HtHMD1), % Head to Head Map Differential is tied
    \+member(Team1, HtHMD2), % ^ ^
    member(Team2, HtHR1). % Head to Head Record is in favor of Team1
compareRecords(>, Record1, Record2) :- % should be restricted to only at the end of stages
    Record1 = record(team(Team1), W1, L1, MD1, HtHMD1, HtHR1),
    Record2 = record(team(Team2), W2, _, MD2, HtHMD2, HtHR2),
    W1 = W2,
    MD1 = MD2,
    \+member(Team2, HtHMD1),
    \+member(Team1, HtHMD2),
    \+member(Team2, HtHR1),
    \+member(Team1, HtHR2),
    endOfStage(W1, L1),
    tieBreakerMatchPlayed(Team1, Team2),
    tieBreakerMatch(Team1, Team2, Team1).
% tie cases
compareRecords(>, Record1, Record2) :-
    Record1 = record(team(Team1), W1, L1, MD1, HtHMD1, HtHR1),
    Record2 = record(team(Team2), W2, _, MD2, HtHMD2, HtHR2),
    W1 = W2,
    MD1 = MD2,
    \+member(Team2, HtHMD1),
    \+member(Team1, HtHMD2),
    \+member(Team2, HtHR1),
    \+member(Team1, HtHR2),
    \+endOfStage(W1, L1),
    % ensure alphabetical order of team names
    Team1 @> Team2.
compareRecords(>, Record1, Record2) :-
    Record1 = record(team(Team1), W1, L1, MD1, HtHMD1, HtHR1),
    Record2 = record(team(Team2), W2, _, MD2, HtHMD2, HtHR2),
    W1 = W2,
    MD1 = MD2,
    \+member(Team2, HtHMD1),
    \+member(Team1, HtHMD2),
    \+member(Team2, HtHR1),
    \+member(Team1, HtHR2),
    endOfStage(W1, L1),
    \+tieBreakerMatchPlayed(Team1, Team2),
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
    findall(Teams, bagof(Team, member(record(Team, _, _, _, [_], [_]), SortedRecords), Teams), GroupedRecords),
    reverse(GroupedRecords, AscendingRecords),
    assignStandings(1, AscendingRecords, Standings), !.


