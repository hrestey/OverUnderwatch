:- [teams].
:- use_module(library(clpfd)).

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
    NewNum #= NumTeams + 1,
    groupTeams(Record2, Record3, Records, NewNum, UpdatedNumTeams, RemainingRecords, GroupedTeams).
groupTeams(Record1, Record2, Records, NumTeams, NumTeams, Records, [team(Team1),team(Team2)]) :-
    Record1 = record(team(Team1), _, _, _, _, _, _),
    Record2 = record(team(Team2), _, _, _, _, _, _).

assignStandings(_, [], []).
assignStandings(Rank, [Record1,Record2|Records], [(Rank, GroupedTeams)|Rest]) :-
    tied(Record1, Record2),
    groupTeams(Record1, Record2, Records, 2, NumTeams, RemainingRecords, GroupedTeams),
    NewRank #= Rank + NumTeams,
    assignStandings(NewRank, RemainingRecords, Rest).
assignStandings(Rank, [Record|Records], [(Rank, [team(Team)])|Rest]) :-
    Record = record(team(Team), _, _, _, _, _, _),
    NewRank #= Rank + 1,
    assignStandings(NewRank, Records, Rest).

teamStandings(Records, Standings) :-
    predsort(compareRecords, Records, SortedRecords),
    reverse(SortedRecords, ReversedSortedRecords),
    assignStandings(1, ReversedSortedRecords, Standings), !.

updateHeadToHeadLists(HeadToHeadMapDiff, HeadToHeadRecord, Wins, Losses, Opponent, [[Opponent, NewMaps]|UpdatedMapDiff], [[Opponent, NewRecord]|UpdatedRecord]):-
    select([Opponent, Maps], HeadToHeadMapDiff, UpdatedMapDiff),
    select([Opponent, Record], HeadToHeadRecord, UpdatedRecord),
    Wins > Losses,
    Diff #= Wins - Losses,
    NewMaps #= Maps + Diff,
    NewRecord #= Record + 1.
updateHeadToHeadLists(HeadToHeadMapDiff, HeadToHeadRecord, Wins, Losses, Opponent, [[Opponent, NewMaps]|UpdatedMapDiff], [[Opponent, NewRecord]|UpdatedRecord]) :-
    select([Opponent, Maps], HeadToHeadMapDiff, UpdatedMapDiff),
    select([Opponent, Record], HeadToHeadRecord, UpdatedRecord),
    Losses > Wins,
    Diff #= Losses - Wins,
    NewMaps #= Maps - Diff,
    NewRecord #= Record - 1.

aWeekOfMatches(Records, [], Records, Standings) :-
    teamStandings(Records, Standings), !.
aWeekOfMatches(StartingRecords, [Match|Schedule], EndingRecords, Standings) :-
    Match = [team(Team1), W1, team(Team2), W2, false],
    select(record(team(Team1), OldW1, OldL1, OldMD1, OldHtHMD1, OldHtHR1, TieBreakers1), StartingRecords, UpdatedStartingRecords),
    select(record(team(Team2), OldW2, OldL2, OldMD2, OldHtHMD2, OldHtHR2, TieBreakers2), UpdatedStartingRecords, UpdatedStartingRecords2),
    W1 > W2, NewW1 #= OldW1 + 1, NewL2 #= OldL2 + 1, % update wins and losses for the teams according to who won
    NewMD1 #= W1 + OldMD1 - W2, NewMD2 #= W2 + OldMD2 - W1, % update the map differential for both teams
    updateHeadToHeadLists(OldHtHMD1, OldHtHR1, W1, W2, Team2, NewHtHMD1, NewHtHR1),
    updateHeadToHeadLists(OldHtHMD2, OldHtHR2, W2, W1, Team1, NewHtHMD2, NewHtHR2),
    aWeekOfMatches([record(team(Team1), NewW1, OldL1, NewMD1, NewHtHMD1, NewHtHR1, TieBreakers1),
        record(team(Team2), OldW2, NewL2, NewMD2, NewHtHMD2, NewHtHR2, TieBreakers2)|UpdatedStartingRecords2], Schedule, EndingRecords, Standings), !.
aWeekOfMatches(StartingRecords, [Match|Schedule], EndingRecords, Standings) :-
    Match = [team(Team1), W1, team(Team2), W2, false],
    select(record(team(Team1), OldW1, OldL1, OldMD1, OldHtHMD1, OldHtHR1, TieBreakers1), StartingRecords, UpdatedStartingRecords),
    select(record(team(Team2), OldW2, OldL2, OldMD2, OldHtHMD2, OldHtHR2, TieBreakers2), UpdatedStartingRecords, UpdatedStartingRecords2),
    W2 > W1, NewW2 #= OldW2 + 1, NewL1 #= OldL1 + 1, % update wins and losses for the teams according to who won
    NewMD1 #= W1 + OldMD1 - W2, NewMD2 #= W2 + OldMD2 - W1, % update the map differential for both teams
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
    NewTieBreakers #= [Team2|OldTieBreakers],
    aWeekOfMatches([record(team(Team1), W, L, MD, HtHMD, HtHR, NewTieBreakers)|UpdatedStartingRecords], Schedule, EndingRecords, Standings), !.
aWeekOfMatches(StartingRecords, [Match|Schedule], EndingRecords, Standings) :-
    Match = [team(Team1), W1, team(Team2), W2, true],
    W2 > W1,
    select(record(team(Team2), W, L, MD, HtHMD, HtHR, OldTieBreakers), StartingRecords, UpdatedStartingRecords),
    NewTieBreakers #= [Team1|OldTieBreakers],
    aWeekOfMatches([record(team(Team2), W, L, MD, HtHMD, HtHR, NewTieBreakers)|UpdatedStartingRecords], Schedule, EndingRecords, Standings), !.

aStageOfMatches(StartingRecords, [Week1, Week2, Week3, Week4, Week5], [Week1Records, Week2Records, Week3Records, Week4Records, EndingRecords], EndingRecords, Standings) :-
    aWeekOfMatches(StartingRecords, Week1, Week1Records, _), !,
    aWeekOfMatches(Week1Records, Week2, Week2Records, _), !,
    aWeekOfMatches(Week2Records, Week3, Week3Records, _), !,
    aWeekOfMatches(Week3Records, Week4, Week4Records, _), !,
    aWeekOfMatches(Week4Records, Week5, EndingRecords, Standings), !.

aggregateHeadToHeadMapDiffs([], [], []).
aggregateHeadToHeadMapDiffs(StageA, StageB, [NewMD|Rest]) :-
    select([Team, MD1], StageA, UpdatedStageA),
    select([Team, MD2], StageB, UpdatedStageB),
    MD3 #= MD1 + MD2,
    NewMD = [Team, MD3],
    aggregateHeadToHeadMapDiffs(UpdatedStageA, UpdatedStageB, Rest).

aggregateHeadToHeadRecords([], [], []).
aggregateHeadToHeadRecords(StageA, StageB, [NewR|Rest]) :-
    select([Team, R1], StageA, UpdatedStageA),
    select([Team, R2], StageB, UpdatedStageB),
    R3 #= R1 + R2,
    NewR = [Team, R3],
    aggregateHeadToHeadRecords(UpdatedStageA, UpdatedStageB, Rest).

aggregateStageRecords(AllStageRecords, OverallRecords) :-
    % find all distinct teams
    setof(Team, V^V1^V2^V3^V4^V5^member(record(team(Team), V, V1, V2, V3, V4, V5), AllStageRecords), Teams),
    aggregateStageRecordsPerTeam(Teams, AllStageRecords, OverallRecords).

aggregateStageRecordsPerTeam([], _, []).
aggregateStageRecordsPerTeam([Team|Teams], AllStageRecords, [TeamOverallRecords|RestOverallRecords]) :-
    findall(Record, (Record = record(team(Team), _, _, _, _, _, _), member(Record, AllStageRecords)), TeamRecords),
    aggregateTeamRecords(TeamRecords, TeamOverallRecords),
    aggregateStageRecordsPerTeam(Teams, AllStageRecords, RestOverallRecords).

aggregateTeamRecords([], []).
aggregateTeamRecords([Record|RestRecords], FinalRecord) :-
    aggregateTeamRecords(RestRecords, Record, FinalRecord).
aggregateTeamRecords([], Record, Record).
aggregateTeamRecords([Record1|Rest], Record2, FinalRecord) :-
    Record1 = record(team(Team), W1, L1, MD1, HtHMD1, HtHR1, _),
    Record2 = record(team(Team), W2, L2, MD2, HtHMD2, HtHR2, _),
    W3 #= W1 + W2,
    L3 #= L1 + L2,
    MD3 #= MD1 + MD2,
    aggregateHeadToHeadMapDiffs(HtHMD1, HtHMD2, HtHMD3),
    aggregateHeadToHeadRecords(HtHR1, HtHR2, HtHR3),
    NewRecord = record(team(Team), W3, L3, MD3, HtHMD3, HtHR3, []),
    aggregateTeamRecords(Rest, NewRecord, FinalRecord).

aFullOWLSeason(StartingRecords, [Stage1Schedule, Stage2Schedule, Stage3Schedule, Stage4Schedule], [Stage1Records, Stage2Records, Stage3Records, Stage4Records], OverallRecords, OverallStandings) :-
    aStageOfMatches(StartingRecords, Stage1Schedule, _, Stage1Records, _),
    aStageOfMatches(StartingRecords, Stage2Schedule, _, Stage2Records, _),
    aStageOfMatches(StartingRecords, Stage3Schedule, _, Stage3Records, _),
    aStageOfMatches(StartingRecords, Stage4Schedule, _, Stage4Records, _),
    % collapse list of lists into list
    append([Stage1Records, Stage2Records, Stage3Records, Stage4Records], AllStageRecords),
    aggregateStageRecords(AllStageRecords, OverallRecords),
    teamStandings(OverallRecords, OverallStandings).


extractJsonIndividualMatches([], []).
extractJsonIndividualMatches([json(Match)|Remaining], [[team(Team1), Score1, team(Team2), Score2, false]|MatchesRest]) :-
    member(team1 = Team1Full, Match),
    member(t1score = StringScore1, Match),
    member(team2 = Team2Full, Match), 
    member(t2score = StringScore2, Match),
    translate(Team1, Team1Full),
    translate(Team2, Team2Full),
    atom_number(StringScore1, Score1Num),
    atom_number(StringScore2, Score2Num),
    ( Score1Num = 0, Score2Num = 0 ->
        print(here), nl, Score1 in 0..4, Score2 in 0..4, ScoreSum #= Score1 + Score2, ScoreSum in 4..5 ;
        Score1 = Score1Num, Score2 = Score2Num ),
    extractJsonIndividualMatches(Remaining, MatchesRest).

feedJsonMatches([], []).
feedJsonMatches([Week|Rest], [Matches|MatchesRest]) :-
    extractJsonIndividualMatches(Week, Matches),
    feedJsonMatches(Rest, MatchesRest).

extractJsonMatches([], []).
extractJsonMatches([Stage|Rest], [Matches|MatchesRest]) :-
    feedJsonMatches(Stage, Matches),
    extractJsonMatches(Rest, MatchesRest).

extractJsonStageWeeks([], []).
extractJsonStageWeeks([Json|Rest], [Weeks|WeeksRest]) :-
    findall(Matches, member(json([matches = Matches]), Json), Weeks),
    extractJsonStageWeeks(Rest, WeeksRest).

extractJsonStages([], []).
extractJsonStages([Json|Rest], [Stage|StagesRest]) :-
    Json = json([stage = Stage]), % extract all the matches from each week in the stage
    extractJsonStages(Rest, StagesRest).

extractScheduleFromJson(Json, Matches) :-
    extractJsonStages(Json, Stages),
    extractJsonStageWeeks(Stages, Weeks),
    extractJsonMatches(Weeks, Matches).

%countTeamPaths(Team, Records, Schedule, FinalStandings, Count) :-
%    .
