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

%record(team(name), Wins, Losses, OverallMapDiff, HeadToHeadMapDiff, HeadToHead, TieBreakers) -> HeadToHeadMapDiff and HeadToHead and TieBreakers have team names in them if the team has a positive record against that team, or if they have won a tie breaker match against them

compareRecords(>, Record1, Record2) :-
    Record1 = record(team(_), W1, _, _, [_], [_], [_]), 
    Record2 = record(team(_), W2, _, _, [_], [_], [_]),
    W1 > W2.
compareRecords(>, Record1, Record2) :-
    Record1 = record(team(_), W1, _, MD1, [_], [_], [_]),
    Record2 = record(team(_), W2, _, MD2, [_], [_], [_]),
    W1 = W2,
    MD1 > MD2.
compareRecords(>, Record1, Record2) :-
    Record1 = record(team(_), W1, _, MD1, HtHMD1, [_], [_]),
    Record2 = record(team(Team2), W2, _, MD2, [_],[_], [_]),
    W1 = W2,
    MD1 = MD2,
    member(Team2, HtHMD1). % Head to Head Map Differential in favor of Team1
compareRecords(>, Record1, Record2) :-
    Record1 = record(team(Team1), W1, _, MD1, HtHMD1, HtHR1, [_]),
    Record2 = record(team(Team2), W2, _, MD2, HtHMD2, [_], [_]),
    W1 = W2,
    MD1 = MD2,
    \+member(Team2, HtHMD1), % Head to Head Map Differential is tied
    \+member(Team1, HtHMD2), % ^ ^
    member(Team2, HtHR1). % Head to Head Record is in favor of Team1
compareRecords(>, Record1, Record2) :- % should be restricted to only at the end of stages
    Record1 = record(team(Team1), W1, L1, MD1, HtHMD1, HtHR1, TieBreakers),
    Record2 = record(team(Team2), W2, _, MD2, HtHMD2, HtHR2, [_]),
    W1 = W2,
    MD1 = MD2,
    \+member(Team2, HtHMD1),
    \+member(Team1, HtHMD2),
    \+member(Team2, HtHR1),
    \+member(Team1, HtHR2),
    endOfStage(W1, L1),
    member(Team2, TieBreakers).
% tie cases
compareRecords(>, Record1, Record2) :-
    Record1 = record(team(Team1), W1, L1, MD1, HtHMD1, HtHR1, [_]),
    Record2 = record(team(Team2), W2, _, MD2, HtHMD2, HtHR2, [_]),
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
    Record1 = record(team(Team1), W1, L1, MD1, HtHMD1, HtHR1, TieBreakers1),
    Record2 = record(team(Team2), W2, _, MD2, HtHMD2, HtHR2, TieBreakers2),
    W1 = W2,
    MD1 = MD2,
    \+member(Team2, HtHMD1),
    \+member(Team1, HtHMD2),
    \+member(Team2, HtHR1),
    \+member(Team1, HtHR2),
    endOfStage(W1, L1),
    \+member(Team2, TieBreakers1),
    \+member(Team1, TieBreakers2),
    Team1 @> Team2.
compareRecords(<, Team1, Team2) :-
    compareRecords(>, Team2, Team1).

% label teams as 1st, 2nd, etc.
assignStandings(_, [], []).
assignStandings(N, [H|T], [(N, H)|Rest]) :-
    N2 is N + 1,
    assignStandings(N2, T, Rest).

tied(Record1, Record2) :-
    Record1 = record(team(Team1), W1, _, MD1, HtHMD1, HtHR1, TieBreakers1),
    Record1 = record(team(Team2), W2, _, MD2, HtHMD1, HtHR2, TieBreakers2),
    \+member(Team2, HtHMD1),
    \+member(Team1, HtHMD2),
    \+member(Team2, HtHR1),
    \+member(Team1, HtHR2),
    \+member(Team2, TieBreakers1),
    \+member(Team1, TieBreakers2).

groupTeams([Record], []) :-
    Record = record(team(Team), _, _, _, [_], [_], [_]).
groupTeams(Record1, Record2, [Record3|Records], NumTeams, UpdatedNumTeams, RemainingRecords, [team(Team1)|GroupedTeams]) :-
    Record1 = record(team(Team1), _, _, _, [_], [_], [_]),
    tied(Record1, Record3),
    NewNum is NumTeams + 1,
    groupTeams(Record2, Record3, Records, NewNum, UpdatedNumTeams, RemainingRecords, GroupedTeams).
groupTeams(Record1, Record2, Records, NumTeams, NumTeams, Records, [team(Team1),team(Team2)|_]).

assignStandings2(_, [], [_]).
assignStandings2(Rank, [Record1,Record2|Records], [(Rank, GroupedTeams)|Rest]) :-
    tied(Record1, Record2),
    groupTeams(Record1, Record2, Records, 2, NumTeams, RemainingRecords, GroupedTeams),
    NewRank is Rank + NumTeams,
    assignStandings2(NewRank, RemainingRecords, Rest).
assignStandings2(Rank, [Record|Records], [(Rank, [team(Team)]|Rest]) :-
    Record = record(team(Team), _, _, _, [_], [_], [_]),
    NewRank is Rank + 1,
    assignStandings2(NewRank, Records, Rest).

teamStandings(Records, Standings) :-
    print(Records), nl,
    predsort(compareRecords, Records, SortedRecords),
    print(SortedRecords), nl,
    % group teams by their records, so we can establish standings
    %findall(Teams, bagof(Team, member(record(Team, A, B, C, D, E, F), SortedRecords), Teams), GroupedRecords),
    reverse(SortedRecords, ReversedSortedRecords),
    print(GroupedRecords), nl,
    %reverse(GroupedRecords, AscendingRecords),
    assignStandings(1, GroupedRecords, Standings), !.


