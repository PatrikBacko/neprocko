-- 7. cvičení 2017-04-04
--
-- Práce s GHCi/WinGHCi
--
-- :cd <dir> - mění working directory
-- :! <cmd>  - pustí shell command cmd
-- :i <name> - zobrazí informace o name
-- :l <file> - načte .hs soubor
-- :main     - pustí main (bude později)
-- :m <mod>  - načte modul mod z knihovny
-- :t <expr> - zobrazí typ výrazu expr
-- :r        - reload
--
--
-- Základní operace
-- > 1 + 2 * 3 / 5
-- 2.2
--
-- > 2 * (-2)   -- Závorky jsou nutné.
-- -4
--
-- > 0 == 0
-- True
--
-- > 1 /= 2
-- True
--
-- Dále <, <=, >, >=
--
-- > True && False
-- False
--
-- > True || False
-- True
--
-- Aplikace (volání) funkcí
--
-- C#        Haskell
-- f(x)      f x
-- f(x,y)    f x y
-- f(g(x))   f (g x)
--
-- > not True
-- False
--
-- > not (not True)
-- True
--
-- > not not True   -- odpovídalo by not(not, True)
-- *chyba*
--
-- > max 1 2
-- 2
--
-- > max 1 (max 2 3)
-- 3
--
-- > max 3 2 * 4
-- 12
--
-- > max 3 (2 * 4)
-- 8
--
-- Vlastní funkce

-- jméno argument1 arguement2 .. argumentN = výraz
pyth a b = sqrt (a * a + b * b)

-- if-then-else
abs' x = if x < 0 then -x else x

-- > abs' (-5)
-- 5
--
-- else větev je povinná, if-then-else je výraz, takže se dá použít např. takhle
absPlus x y = (if x < 0 then -x else x) + y

-- Seznamy
-- [] prázdný seznam, x:xs neprázdný seznam, x hlava, xs tělo
--
-- > length (1:2:3:4:5:[])
-- 5
--
-- Podobně jako Prolog máme syntaktickou zkratku
--
-- > length [1,2,3,4,5]
-- 5
--
-- ++ je append, !! je indexování
-- > [1,2,3] ++ [4,5,6]
-- [1,2,3,4,5,6]
--
-- > [1,2,3] !! 1
-- 2
--
-- Další funkce: head, tail, last, init, null, reverse
--
-- > take 3 [1,2,3,4,5]
-- [1,2,3]
--
-- > drop 3 [1,2,3,4,5]
-- [4,5]
--
-- > sum [1,2,3,4]
-- 10
--
-- > product [1,2,3,4]  -- 4!
-- 24
--
-- Vytváření seznamů
--
-- > [1..5]
-- [1,2,3,4,5]
--
-- > [1,3..10]
-- [1,3,5,7,9]
--
-- > ['a', 'c' .. 'z']
-- "acegikmoqsuwy"
--
-- > [10,9..1]
-- [10,9,8,7,6,5,4,3,2,1]
--
-- Transformace na seznamech
--
-- > [x + 1 | x <- [1..5]]
-- [2,3,4,5,6]
--
-- > [x * y | x <- [1..3], y <- [2..5]]
-- [2,3,4,5,4,6,8,10,6,9,12,15]
--
-- > [x | x <- [1..20], gcd x 20 == 1]
-- [1,3,7,9,11,13,17,19]
--
--
-- Pozor: všechny prvky seznamu musejí mít stejný typ
--
-- > [1, True]
-- *chyba*
--
--
-- n-tice (tuples)
--
-- Narozdíl od seznamů mají fixní délku, prvky mohou mít různé typy.
--
-- > (1, True)
-- (1, True)
--
-- > fst (1, True)
-- 1
--
-- > snd (1, True)
-- True
--
-- Pro větší n-tice: pattern matching (bude později).
--
-- Kartézský součin
--
-- Další operace: takeWhile, dropWhile, zip, filter, map, foldr

times xs ys = [(x, y) | x <- xs, y <- ys]

-- Na procvičení: hledání pythagorejských trojic, klouzavý průměr,
-- nejbližší vyšší mocnina dvojky (pomocí seznamů), hledání výskytu v řetěci
