:- [teams].

endOfStage(Wins, Losses) :-
    Sum is Wins + Losses,
    Sum = 10. 

%record(team(name), Wins, Losses, OverallMapDiff, HeadToHeadMapDiff, HeadToHead, TieBreakers) -> HeadToHeadMapDiff and HeadToHead and TieBreakers have team names in them if the team has a positive record against that team, or if they have won a tie breaker match against them

compareRecords(>, Record1, Record2) :-
    Record1 = record(team(_), W1, _, _, _, _, _), 
    Record2 = record(team(_), W2, _, _, _, _, _),
    W1 > W2.
compareRecords(>, Record1, Record2) :-
    Record1 = record(team(_), W1, _, MD1, _, _, _),
    Record2 = record(team(_), W2, _, MD2, _, _, _),
    W1 = W2,
    MD1 > MD2.
compareRecords(>, Record1, Record2) :-
    Record1 = record(team(_), W1, _, MD1, HtHMD1, _, _),
    Record2 = record(team(Team2), W2, _, MD2, _,_, _),
    W1 = W2,
    MD1 = MD2,
    member([Team2, _], HtHMD1). % Head to Head Map Differential in favor of Team1
compareRecords(>, Record1, Record2) :-
    Record1 = record(team(Team1), W1, _, MD1, HtHMD1, HtHR1, _),
    Record2 = record(team(Team2), W2, _, MD2, HtHMD2, _, _),
    W1 = W2,
    MD1 = MD2,
    \+member([Team2, _], HtHMD1), % Head to Head Map Differential is tied
    \+member([Team1, _], HtHMD2), % ^ ^
    member([Team2, _], HtHR1). % Head to Head Record is in favor of Team1
compareRecords(>, Record1, Record2) :- % should be restricted to only at the end of stages
    Record1 = record(team(Team1), W1, _, MD1, HtHMD1, HtHR1, TieBreakers),
    Record2 = record(team(Team2), W2, _, MD2, HtHMD2, HtHR2, _),
    W1 = W2,
    MD1 = MD2,
    \+member([Team2, _], HtHMD1),
    \+member([Team1, _], HtHMD2),
    \+member([Team2, _], HtHR1),
    \+member([Team1, _], HtHR2),
    member(Team2, TieBreakers).
% tie case
compareRecords(>, Record1, Record2) :-
    tied(Record1, Record2),
    Record1 = record(team(Team1), _, _, _, _, _, _),
    Record2 = record(team(Team2), _, _, _, _, _, _),
    % ensure alphabetical order of team names
    Team2 @> Team1.
compareRecords(<, Team1, Team2) :-
    compareRecords(>, Team2, Team1).

tied(Record1, Record2) :-
    Record1 = record(team(Team1), W1, _, MD1, HtHMD1, HtHR1, TieBreakers1),
    Record2 = record(team(Team2), W1, _, MD1, HtHMD2, HtHR2, TieBreakers2),
    \+member([Team2, _], HtHMD1),
    \+member([Team1, _], HtHMD2),
    \+member([Team2, _], HtHR1),
    \+member([Team1, _], HtHR2),
    \+member(Team2, TieBreakers1),
    \+member(Team1, TieBreakers2).

groupTeams(Record1, Record2, [Record3|Records], NumTeams, UpdatedNumTeams, RemainingRecords, [team(Team1)|GroupedTeams]) :-
    Record1 = record(team(Team1), _, _, _, _, _, _),
    tied(Record1, Record3),
    NewNum is NumTeams + 1,
    groupTeams(Record2, Record3, Records, NewNum, UpdatedNumTeams, RemainingRecords, GroupedTeams).
groupTeams(Record1, Record2, Records, NumTeams, NumTeams, Records, [team(Team1),team(Team2)]) :-
    Record1 = record(team(Team1), _, _, _, _, _, _),
    Record2 = record(team(Team2), _, _, _, _, _, _).

assignStandings(_, [], []).
assignStandings(Rank, [Record1,Record2|Records], [(Rank, GroupedTeams)|Rest]) :-
    tied(Record1, Record2),
    groupTeams(Record1, Record2, Records, 2, NumTeams, RemainingRecords, GroupedTeams),
    NewRank is Rank + NumTeams,
    assignStandings(NewRank, RemainingRecords, Rest).
assignStandings(Rank, [Record|Records], [(Rank, [team(Team)])|Rest]) :-
    Record = record(team(Team), _, _, _, _, _, _),
    NewRank is Rank + 1,
    assignStandings(NewRank, Records, Rest).

teamStandings(Records, Standings) :-
    predsort(compareRecords, Records, SortedRecords),
    reverse(SortedRecords, ReversedSortedRecords),
    assignStandings(1, ReversedSortedRecords, Standings), !.

aWeekOfMatches(_, [], _).
aWeekOfMatches(StartingRecords, [Match|Schedule], EndingRecords) :-
    Match = [team(Team1), W1, team(Team2), W2, TieBreaker],
    select().

