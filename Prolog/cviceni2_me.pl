% 2. cvičení, 2017-02-27

% Kromě jednoduchých atomů (konstant) můžeme v Prologu také vytvářet složené
% termy.

% Operace na dvojicích.
first(pair(X, Y), X).
second(pair(X, Y), Y).

% first(pair(pair(1, 2), 3), R).
% R = pair(1,2).

% Unárně reprezentovaná čísla.

% nat(X)
% X je přirozené číslo
nat(0).
nat(s(X)) :- nat(X).

% Jak Prolog vyhodnocuje dotazy? Unifikace a backtracking!
%
% Když se Prolog snaží splnit nějaký dotaz a má na výběr více možností
% (predikát definovaný pomocí více než jedné klauzule), zkusí postupně
% ty klauzule, jejichž hlava pasuje na dotaz.
%
% Hlava klauzule pasuje na dotaz, pokud je lze unifikovat, tj. najít hodnoty
% proměnných tž. po dosazení jsou hlava a dotaz stejné. Prolog vždy hledá
% neobecnější unifikaci, která neobsahuje žádné zbytečné vazby.
%
% X = X.
% p(X) = Y.
% f(X, Y) = g(X). % false.
% f(X, b) = f(a, Y). % X = a, Y = b.

vertical(line(point(X, Y), point(X, Z))).
horizontal(line(point(X, Y), point(Z, Y))).

% V těle klauzule se také může objevit predikát, který právě definujeme.
% Jsou tedy možné rekurzivní definice.
%
% Klauzule se zkoušejí v pořadí, v jakém jsou zapsané v programu. Stejně tak
% se vyhodnocuje tělo klauzule.
%
% Pokud nějaký poddotaz skončí neúspěchem, Prolog se vrátí na poslední místo,
% kde existuje nějaká volba a zkusí jinou možnost.

% Méně nebo rovno
leq(0, Y) :- nat(Y).
leq(s(X), s(Y)) :- leq(X, Y).

% Alternativní definice
leq2(X, X) :- nat(X).
leq2(X, s(Y)) :- leq2(X, Y).

% Méně než.
lt(0, s(Y)) :- nat(Y).
lt(s(X), s(Y)) :- lt(X, Y).

% Sčítání.
add(0, Y, Y) :- nat(Y).
add(s(X), Y, s(Z)) :-
  add(X, Y, Z).

%Nasobenie
mul(0, Y, 0) :- nat(Y).
mul(s(X),Y,R) :- mul(X,Y,Z), add(Z,Y,R).

%delenie, X/Y = R
dif(X,Y,R, Rem) :- mul(R,Y,X).

%delenie, X/Y = R, so zbytkom Rem
difWithRem(X,Y,R, Rem) :- mul(R,Y,Z), add(Z,Rem,X).





% Pomocné predikáty.
toNat(N, R) :-
    integer(N),
    toNat_(N, R).
  
  toNat_(N, R) :- N > 0 ->
    (N2 is N - 1, toNat_(N2, R2), R = s(R2));
    R = 0.
  
  fromNat(0, 0).
  fromNat(s(N), R) :-
    fromNat(N, R2),
    R is R2 + 1.
  