len([], 0).
len([_|Y],R) :- len(Y, R1), R is R1 + 1.

split([X|Y], X, Y).
split([_|Y], A, B) :- split(Y,A,B).

combination(0, _, []).
%combination(N, X, R) :-