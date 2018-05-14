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
    member([Team2, A], HtHMD1), A > 0. % Head to Head Map Differential in favor of Team1
compareRecords(>, Record1, Record2) :-
    Record1 = record(team(Team1), W1, _, MD1, HtHMD1, HtHR1, _),
    Record2 = record(team(Team2), W2, _, MD2, HtHMD2, _, _),
    W1 = W2,
    MD1 = MD2,
    member([Team2, 0], HtHMD1), % Head to Head Map Differential is tied
    member([Team1, 0], HtHMD2),
    member([Team2, A], HtHR1), A > 0. % Head to Head Record is in favor of Team1
compareRecords(>, Record1, Record2) :- % should be restricted to only at the end of stages
    Record1 = record(team(Team1), W1, _, MD1, HtHMD1, HtHR1, TieBreakers),
    Record2 = record(team(Team2), W2, _, MD2, HtHMD2, HtHR2, _),
    W1 = W2,
    MD1 = MD2,
    member([Team2, 0], HtHMD1),
    member([Team1, 0], HtHMD2),
    member([Team2, 0], HtHR1),
    member([Team1, 0], HtHR2),
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
    member([Team2, 0], HtHMD1),
    member([Team1, 0], HtHMD2),
    member([Team2, 0], HtHR1),
    member([Team1, 0], HtHR2),
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

updateHeadToHeadLists(HeadToHeadMapDiff, HeadToHeadRecord, TieBreakers, Wins, Losses, Opponent, false, [[team(Opponent), NewMaps]|UpdatedMapDiff], [[team(Opponent), NewRecord]|UpdatedRecord],
    TieBreakers) :-
    select(HeadToHeadMapDiff, [team(Opponent), Maps], UpdatedMapDiff),
    select(HeadToHeadRecord, [team(Opponent), Record], UpdatedRecord),
    Wins > Losses,
    NewMaps is Maps + Wins - Losses,
    NewRecord is Record + 1.
updateHeadToHeadLists(HeadToHeadMapDiff, HeadToHeadRecord, TieBreakers, Wins, Losses, Opponent, false, [[team(Opponent), NewMaps]|UpdatedMapDiff], [[team(Opponent), NewRecord]|UpdatedRecord],
    TieBreakers) :-
    select(HeadToHeadMapDiff, [team(Opponent), Maps], UpdatedMapDiff),
    select(HeadToHeadRecord, [team(Opponent), Record], UpdatedRecord),
    Losses > Wins,
    NewMaps is Maps + Wins - Losses,
    NewRecord is Record - 1.

aWeekOfMatches(Records, [], Records, Standings) :-
    teamStandings(Records, Standings).
aWeekOfMatches(StartingRecords, [Match|Schedule], EndingRecords, Standings) :-
    Match = [team(Team1), W1, team(Team2), W2, false],
    select(StartingRecords, record(team(Team1), OldW1, OldL1, OldMD1, OldHtHMD1, OldHtHR1, OldTieBreakers1), UpdatedStartingRecords),
    select(UpdatedStartingRecords, record(team(Team2), OldW2, OldL2, OldMD2, OldHtHMD2, OldHtHR2, OldTieBreakers2), UpdatedStartingRecords2),
    W1 > W2, NewW1 is OldW1 + 1, NewL2 is OldL2 + 1, % update wins and losses for the teams according to who won
    NewMD1 is W1 + OldMD1 - W2, NewMD2 is W2 + OldMD2 - W1, % update the map differential for both teams
    updateHeadToHeadLists(OldHtHMD1, OldHtHR1, OldTieBreakers1, W1, W2, Team2, TieBreaker, NewHtHMD1, NewHtHR1, NewTieBreakers1),
    updateHeadToHeadLists(OldHtHMD2, OldHtHR2, OldTieBreakers2, W2, W1, Team1, TieBreaker, NewHtHMD2, NewHtHR2, NewTieBreakers2),
    aWeekOfMatches([record(team(Team1), NewW1, OldL1, NewMD1, NewHtHMD1, NewHtHR1, NewTieBreakers1),
        record(team(Team2), OldW2, NewL2, NewMD2, NewHtHMD2, NewHtHR2, NewTieBreakers2)|UpdatedStartingRecords2], Schedule, EndingRecords, Standings).
aWeekOfMatches(StartingRecords, [Match|Schedule], EndingRecords, Standings) :-
    Match = [team(Team1), W1, team(Team2), W2, false],
    select(StartingRecords, record(team(Team1), OldW1, OldL1, OldMD1, OldHtHMD1, OldHtHR1, TieBreakers1), UpdatedStartingRecords),
    select(UpdatedStartingRecords, record(team(Team2), OldW2, OldL2, OldMD2, OldHtHMD2, OldHtHR2, TieBreakers2), UpdatedStartingRecords2),
    W2 > W2, NewW2 is OldW2 + 1, NewL1 is OldL1 + 1, % update wins and losses for the teams according to who won
    NewMD1 is W1 + OldMD1 - W2, NewMD2 is W2 + OldMD2 - W1, % update the map differential for both teams
    updateHeadToHeadLists(OldHtHMD1, OldHtHR1, W1, W2, Team2, NewHtHMD1, NewHtHR1),
    updateHeadToHeadLists(OldHtHMD2, OldHtHR2, W2, W1, Team1, NewHtHMD2, NewHtHR2),
    aWeekOfMatches([record(team(Team1), OldW1, NewL1, NewMD1, NewHtHMD1, NewHtHR1, TieBreakers1),
        record(team(Team2), NewW2, OldL2, NewMD2, NewHtHMD2, NewHtHR2, TieBreakers2)|UpdatedStartingRecords2], Schedule, EndingRecords, Standings).
aWeekOfMatches(StartingRecords, [Match|Schedule], EndingRecords, Standings) :-
    Match = [team(Team1), W1, team(Team2), W2, true],
    W1 > W2,
    select(StartingRecords, record(team(Team1), W, L, MD, HtHMD, HtHR, OldTieBreakers), UpdatedStartingRecords),
    NewTieBreakers is [Team2|OldTieBreakers],
    aWeekOfMatches([record(team(Team1), W, L, MD, HtHMD, HtHR, NewTieBreakers)|UpdatedStartingRecords], Schedule, EndingRecords, Standings).
aWeekOfMatches(StartingRecords, [Match|Schedule], EndingRecords, Standings) :-
    Match = [team(Team1), W1, team(Team2), W2, true],
    W2 > W1,
    select(StartingRecords, record(team(Team2), W, L, MD, HtHMD, HtHR, OldTieBreakers), UpdatedStartingRecords),
    NewTieBreakers is [Team1|OldTieBreakers],
    aWeekOfMatches([record(team(Team2), W, L, MD, HtHMD, HtHR, NewTieBreakers)|UpdatedStartingRecords], Schedule, EndingRecords, Standings).
