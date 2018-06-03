:- [teams].

endOfStage(Wins, Losses) :-
    Sum is Wins + Losses,
    Sum = 10.
endOfSeason(Wins, Losses) :-
    Sum is Wins + Losses,
    Sum = 40.

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
    Record1 = record(team(Team1), W1, L1, MD1, HtHMD1, HtHR1, TieBreakers1),
    endOfStage(W1, L1),
    Record2 = record(team(Team2), W1, _, MD1, HtHMD2, HtHR2, TieBreakers2),
    member([Team2, 0], HtHMD1),
    member([Team1, 0], HtHMD2),
    member([Team2, 0], HtHR1),
    member([Team1, 0], HtHR2),
    \+member(Team2, TieBreakers1),
    \+member(Team1, TieBreakers2).
tied(Record1, Record2) :-
    Record1 = record(team(Team1), W1, L1, MD1, HtHMD1, HtHR1, TieBreakers1),
    endOfSeason(W1, L1),
    Record2 = record(team(Team2), W1, _, MD1, HtHMD2, HtHR2, TieBreakers2),
    member([Team2, 0], HtHMD1),
    member([Team1, 0], HtHMD2),
    member([Team2, 0], HtHR1),
    member([Team1, 0], HtHR2),
    \+member(Team2, TieBreakers1),
    \+member(Team1, TieBreakers2).
tied(Record1, Record2) :-
    Record1 = record(team(_), W1, _, MD1, _, _, _),
    Record2 = record(team(_), W1, _, MD1, _, _, _).

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

updateHeadToHeadLists(HeadToHeadMapDiff, HeadToHeadRecord, Wins, Losses, Opponent, [[Opponent, NewMaps]|UpdatedMapDiff], [[Opponent, NewRecord]|UpdatedRecord]):-
    select([Opponent, Maps], HeadToHeadMapDiff, UpdatedMapDiff),
    select([Opponent, Record], HeadToHeadRecord, UpdatedRecord),
    Wins > Losses,
    Diff is Wins - Losses,
    NewMaps is Maps + Diff,
    NewRecord is Record + 1.
updateHeadToHeadLists(HeadToHeadMapDiff, HeadToHeadRecord, Wins, Losses, Opponent, [[Opponent, NewMaps]|UpdatedMapDiff], [[Opponent, NewRecord]|UpdatedRecord]) :-
    select([Opponent, Maps], HeadToHeadMapDiff, UpdatedMapDiff),
    select([Opponent, Record], HeadToHeadRecord, UpdatedRecord),
    Losses > Wins,
    Diff is Losses - Wins,
    NewMaps is Maps - Diff,
    NewRecord is Record - 1.

aWeekOfMatches(Records, [], Records, Standings) :-
    teamStandings(Records, Standings), !.
aWeekOfMatches(StartingRecords, [Match|Schedule], EndingRecords, Standings) :-
    Match = [team(Team1), W1, team(Team2), W2, false],
    select(record(team(Team1), OldW1, OldL1, OldMD1, OldHtHMD1, OldHtHR1, TieBreakers1), StartingRecords, UpdatedStartingRecords),
    select(record(team(Team2), OldW2, OldL2, OldMD2, OldHtHMD2, OldHtHR2, TieBreakers2), UpdatedStartingRecords, UpdatedStartingRecords2),
    W1 > W2, NewW1 is OldW1 + 1, NewL2 is OldL2 + 1, % update wins and losses for the teams according to who won
    NewMD1 is W1 + OldMD1 - W2, NewMD2 is W2 + OldMD2 - W1, % update the map differential for both teams
    updateHeadToHeadLists(OldHtHMD1, OldHtHR1, W1, W2, Team2, NewHtHMD1, NewHtHR1),
    updateHeadToHeadLists(OldHtHMD2, OldHtHR2, W2, W1, Team1, NewHtHMD2, NewHtHR2),
    aWeekOfMatches([record(team(Team1), NewW1, OldL1, NewMD1, NewHtHMD1, NewHtHR1, TieBreakers1),
        record(team(Team2), OldW2, NewL2, NewMD2, NewHtHMD2, NewHtHR2, TieBreakers2)|UpdatedStartingRecords2], Schedule, EndingRecords, Standings), !.
aWeekOfMatches(StartingRecords, [Match|Schedule], EndingRecords, Standings) :-
    Match = [team(Team1), W1, team(Team2), W2, false],
    select(record(team(Team1), OldW1, OldL1, OldMD1, OldHtHMD1, OldHtHR1, TieBreakers1), StartingRecords, UpdatedStartingRecords),
    select(record(team(Team2), OldW2, OldL2, OldMD2, OldHtHMD2, OldHtHR2, TieBreakers2), UpdatedStartingRecords, UpdatedStartingRecords2),
    W2 > W1, NewW2 is OldW2 + 1, NewL1 is OldL1 + 1, % update wins and losses for the teams according to who won
    NewMD1 is W1 + OldMD1 - W2, NewMD2 is W2 + OldMD2 - W1, % update the map differential for both teams
    updateHeadToHeadLists(OldHtHMD1, OldHtHR1, W1, W2, Team2, NewHtHMD1, NewHtHR1),
    updateHeadToHeadLists(OldHtHMD2, OldHtHR2, W2, W1, Team1, NewHtHMD2, NewHtHR2),
    aWeekOfMatches([record(team(Team1), OldW1, NewL1, NewMD1, NewHtHMD1, NewHtHR1, TieBreakers1),
        record(team(Team2), NewW2, OldL2, NewMD2, NewHtHMD2, NewHtHR2, TieBreakers2)|UpdatedStartingRecords2], Schedule, EndingRecords, Standings), !.
    % These cases are for when the match is a tiebreaker match. As things are currently, I believe that they don't affect any part of the standings beyond simply stating who owns the tie breaker, so
    % I only updated the last list in the record.
aWeekOfMatches(StartingRecords, [Match|Schedule], EndingRecords, Standings) :-
    Match = [team(Team1), W1, team(Team2), W2, true],
    W1 > W2,
    select(record(team(Team1), W, L, MD, HtHMD, HtHR, OldTieBreakers), StartingRecords, UpdatedStartingRecords),
    NewTieBreakers is [Team2|OldTieBreakers],
    aWeekOfMatches([record(team(Team1), W, L, MD, HtHMD, HtHR, NewTieBreakers)|UpdatedStartingRecords], Schedule, EndingRecords, Standings), !.
aWeekOfMatches(StartingRecords, [Match|Schedule], EndingRecords, Standings) :-
    Match = [team(Team1), W1, team(Team2), W2, true],
    W2 > W1,
    select(record(team(Team2), W, L, MD, HtHMD, HtHR, OldTieBreakers), StartingRecords, UpdatedStartingRecords),
    NewTieBreakers is [Team1|OldTieBreakers],
    aWeekOfMatches([record(team(Team2), W, L, MD, HtHMD, HtHR, NewTieBreakers)|UpdatedStartingRecords], Schedule, EndingRecords, Standings), !.

aStageOfMatches(StartingRecords, [Week1, Week2, Week3, Week4, Week4], [Week1Records, Week2Records, Week3Records, Week4Records, EndingRecords], EndingRecords, Standings) :-
    aWeekOfMatches(StartingRecords, Week1, Week1Records, _), !,
    aWeekOfMatches(Week1Standings, Week2, Week2Records, _), !,
    aWeekOfMatches(Week2Standings, Week3, Week3Records, _), !,
    aWeekOfMatches(Week3Standings, Week4, Week4Records, _), !,
    aWeekOfMatches(Week4Standings, Week5, EndingRecords, Standings), !.

aggregateStageRecords([], StageRecords,  OverallRecords) :-
    OverallRecords = StageRecords.
aggregateStageRecords([Stage|Remaining), OverallRecords) :-
    select(record(team(_), _, _, _, _, _, _), Stage, UpdatedStage),
    aggregateStageRecords([UpdatedStage|Remaining], [record(team(_), _, _, _, _, _, _)], OverallRecords).
aggregateStageRecords([Stage|Remaining], [record(team(Team), W2, L2, MD2, HtHMD2, HtHR2, TieBreakers2)|Records], OverallRecords) :-
    select(record(team(Team), W1, L1, MD1, HtHMD1, HtHR1, TieBreakers1), Stage, []),
    NewRecord is record(team(Team), W1 + W2, L1 + L2, MD1 + MD2, %Somehow need to aggregate Head to Head stuff and the Tiebreakers. Not sure yet
    .

aFullOWLSeason(StartingRecords, [Stage1Schedule, Stage2Schedule, Stage3Schedule, Stage4Schedule], [Stage1Records, Stage2Records, Stage3Records, Stage4Records], OverallRecords, OverallStandings) :-
    aStageOfMatches(StartingRecords, Stage1Schedule, _, Stage1Records, _),
    aStageOfMatches(StartingRecords, Stage2Schedule, _, Stage2Records, _),
    aStageOfMatches(StartingRecords, Stage3Schedule, _, Stage3Records, _),
    aStageOfMatches(StartingRecords, Stage4Schedule, _, Stage4Records, _),
    aggregateStageRecords([Stage1Records, Stage2Records, Stage3Records, Stage4Records], OverallRecords),
    teamStandings(OverallRecords, OverallStandings).
