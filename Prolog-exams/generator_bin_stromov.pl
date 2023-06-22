% maptree(+P, ?T)

maptree(_, nil) :- !.

maptree(P, b(T1, V, T2)) :-
    call(P, V),
    maptree(P, T1),
    maptree(P, T2).


% size(-Tree, +Number_of_nodes, +Height)

size(b(T1, _, T2), N, H1) :-
    N > 0,
    H1 > -1,
    N > H1,
    NN is N-1,
    between(0, NN, V),
    H2 is H1 - 1,
    V1 is NN - V,
    size(T2, V1, H2),
    size(T1, V, H2). 

size(nil, 0, -1):- !.
size(nil, 0, _):- !.
