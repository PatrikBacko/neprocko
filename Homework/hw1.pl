% 1. domácí úloha
%
% a) Implementujte logaritmus o základu 2 (dolní celou část) na unárně
% reprezentovaných číslech.
%
% logtwo(+N, ?Vysledek)
%
% Nápověda: Může se vám hodit pomocný predikát pro půlení.
%
% logtwo(0, R).
% false.
%
% logtwo(s(s(s(0))), R).
% R = s(0).
%
% logtwo(s(s(s(s(0)))), R).
% R = s(s(0)).
%

% half(+N, ?vysledek)

half(0, 0).
half(s(0), 0).
half(s(s(N)), s(R)) :- half(N, R).

% logtwo(+N, ?Vysledek)

logtwo(s(0),0).
logtwo(N,s(R)) :- half(N,X), less(X,N), logtwo(X,R).

% b) Implementujte predikát, který spočte n-té Fibonacciho číslo lépe než
% v exponenciálním čase (ideálně pouze lineárně mnoho sčítání).
%
% fib(+N, ?Vysledek)
%
% Nápověda: Zkuste nejdřív implementovat obecnější predikát, kde si můžete
% zvolit počáteční čísla.
%
% F_0 = 4
% F_1 = 5
% F_2 = 4 + 5 = 9
% F_3 = 5 + 9 = 14
%
% generalizedFib(3, 4, 5, R).
% R = 14.

%Successor aritmetika
%fibSuccessor(+N, ?Vysledek)

fibSuccessor_1(s(0),s(0),0).
fibSuccessor_1(s(N),R,R1) :- fibSuccessor_1(N,R1,R2), add(R1,R2,R).
fibSuccessor(0,0).
fibSuccessor(N,R) :- fibSuccessor_1(N,R,_).

%Normálna aritmetika
%fib(+N, ?Vysledek)

fib_1(1,1,0).
fib_1(N,R,R1) :- N >= 1, N1 is N - 1, fib_1(N1,R1,R2), R is R1 + R2.
fib(0,0).
fib(N,R) :- fib_1(N,R,_).

% c) (BONUSOVÁ ÚLOHA) Implementuje predikát pro sčítání dvou binárních čísel.
%
% Můžete použít např. následující reprezentaci:
%
% 13[dec] = 1101[bin] = b(1, b(0, b(1, b(1, e))))
%
% Příklad použití:
% addBin(b(1, b(0, b(1, e))), b(1, b(1, b(0, b(1, e)))), R).
% R = b(0, b(0, b(0, b(0, b(1, e))))).
%
% resp.
%
% addBin([1, 0, 1], [1, 1, 0, 1], R).
% R = [0, 0, 0, 0, 1].

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

nat(0).
nat(s(X)) :- nat(X).

add(0, Y, Y) :- nat(Y).
add(s(X), Y, s(Z)) :-
  add(X, Y, Z).

less(0, s(Y)) :- nat(Y).
less(s(X), s(Y)) :- less(X, Y).
