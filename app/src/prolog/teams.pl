team(seoul).
team(london).
team(newyork).
team(dallas).
team(shanghai).
team(lavaliant).
team(lagladiators).
team(boston).
team(florida).
team(houston).
team(sanfrancisco).
team(philadelphia).

translate(seoul, 'Seoul Dynasty'). %format is team(internal name, external name)
translate(london, 'London Spitfire').
translate(newyork, 'New York Excelsior').
translate(dallas, 'Dallas Fuel').
translate(shanghai, 'Shanghai Dragons').
translate(lavaliant, 'Los Angeles Valiant').
translate(lagladiators, 'Los Angeles Gladiators').
translate(boston, 'Boston Uprising').
translate(florida, 'Florida Mayhem').
translate(houston, 'Houston Outlaws').
translate(sanfrancisco, 'San Francisco Shock').
translate(philadelphia, 'Philadelphia Fusion').

%possibleScores(S1, S2, Sum)
possibleScores(0, 4, 4).
possibleScores(1, 3, 4).
possibleScores(2, 3, 5).
possibleScores(4, 0, 4).
possibleScores(3, 1, 4).
possibleScores(3, 2, 5).
possibleScores(2, 1, 3).
possibleScores(1, 2, 3).
possibleScores(0, 2, 2).
possibleScores(0, 3, 3).
possibleScores(3, 0, 3).
possibleScores(2, 0, 2).
