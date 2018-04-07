
predsort2(P, L, R) :-
    '$skip_list'(N, L, Tail),
    (   Tail == []
    ->  predsort2(P, N, L, _, R1),
        R = R1
    ;   must_be(L, list)
    ).

predsort2(P, 2, [X1, X2|L], L, R) :-
    !,
    call(P, Delta, X1, X2),
    mysort2(Delta, X1, X2, R).
predsort2(_, 1, [X|L], L, [X]) :- !.
predsort2(_, 0, L, L, []) :- !.
predsort2(P, N, L1, L3, R) :-
    N1 is N // 2,
    plus(N1, N2, N),
    predsort2(P, N1, L1, L2, R1),
    predsort2(P, N2, L2, L3, R2),
    predmerge2(P, R1, R2, R).

mysort2(<, X1, X2, [X1, X2]).
mysort2(=, X1, _,  [X1]).
mysort2(>, X1, X2, [X2, X1]).

predmerge2(_, [], R, R) :- !.
predmerge2(_, R, [], R) :- !.
predmerge2(P, [H1|T1], [H2|T2], Result) :-
    call(P, Delta, H1, H2),
    !,
    predmerge2(Delta, P, H1, H2, T1, T2, Result).

predmerge2(>, P, H1, H2, T1, T2, [H2|R]) :-
    predmerge2(P, [H1|T1], T2, R).
% THIS WAS THE CHANGE, [H1|R] -> [H1,H2|R] in header
predmerge2(=, P, H1, H2, T1, T2, [H1,H2|R]) :-
    predmerge2(P, T1, T2, R).
predmerge2(<, P, H1, H2, T1, T2, [H1|R]) :-
    predmerge2(P, T1, [H2|T2], R).

