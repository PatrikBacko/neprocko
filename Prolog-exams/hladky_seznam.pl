test(X,Y) :- 
    nonvar(X),
    nonvar(Y),
    integer(X),
    integer(Y),
    X1 is X-1,
    X2 is X+1,
    between(X1, X2, Y),
    !. 

test(X, Y) :-
    var(Y), !,
    integer(X),
    X1 is X-1,
    X2 is X+1,
    between(X1, X2, Y).

test(X, Y) :-
    var(X), !,
    integer(Y),
    Y1 is Y-1,
    Y2 is Y+1,
    between(Y1, Y2, X).
    

%hladky(?Xs)
hladky([]).
hladky([_]).
hladky([X,Y|Xs]) :-
    hladky([Y|Xs]),
    test(X,Y).

hladka(Xs) :- 
    use_module(library(clpfd)),
    transpose(Xs, Ys),
    hladka_(Xs, Ys).

hladka_([], []).

hladka_([X|Xs], []) :-
    hladky(X),
    hladka_(Xs, []).

hladka_([], [X|Xs]) :-
    hladky(X),
    hladka_(Xs, []).

hladka_([X|Xs], [Y|Ys]) :-
    hladky(X),
    hladky(Y),
    hladka_(Xs, Ys).