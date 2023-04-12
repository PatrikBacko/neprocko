-- 4. úloha
--
-- 1) Implementujte kódování a dekódování RLE (https://en.wikipedia.org/wiki/Run-length_encoding):

-- >>> rleEncode "hello"
-- [(1,'h'),(1,'e'),(2,'l'),(1,'o')]

rleEncode' :: (Eq a) => Int -> a -> [a] -> [(Int, a)]
rleEncode' count prev [] = [(count, prev)]
rleEncode' count prev (x:xs)
    | x == prev = rleEncode' (count + 1) prev xs
    | otherwise = (count, prev) : rleEncode' 1 x xs

rleEncode :: (Eq a) => [a] -> [(Int, a)]
rleEncode (x:xs) = rleEncode' 1 x xs

-- >>> rleDecode [(1,'h'),(1,'e'),(2,'l'),(1,'o')]
-- "hello"

rleDecode :: [(Int, a)] -> [a]
rleDecode [] = []
-- rleDecode ((n, x):xs) = replicate n x ++ rleDecode xs
rleDecode ((n, x):xs) 
    | n > 0 = x : rleDecode ((n-1, x):xs)
    | otherwise = rleDecode xs

-- 2) Definujte nekonečný seznam všech prvočísel. Pokuste se o efektivní řešení.
-- Poté pomocí něj definujte funkci, která v daném rozsahu najde dvojici po sobě
-- jdoudích prvočísel s maximálním rozdílem. Pokud je jich více, vrátí první z nich.

-- >>> take 5 primes
-- [2,3,5,7,11]

isPrime :: Integer -> Bool
isPrime n = all (\x -> n `mod` x /= 0) [2..(n-1)]

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) = x : sieve [y| y <- xs, y `mod` x /= 0]

primes :: [Integer]
-- primes = [x| x <- [2..], isPrime x]
primes = 2 : 3 : sieve [5,7..]

-- primes :: [Integer]
-- primes = 2: 3: sieve (tail primes) [5,7..]
--  where 
--   sieve (p:ps) xs = h ++ sieve ps [x | x <- t, x `rem` p /= 0]
--                   where (h,~(_:t)) = span (< p*p) xs
-- >>> gap 1000
-- (887, 907)

gap' :: [Integer] -> Integer -> (Integer, Integer) -> (Integer, Integer)
gap' [] max res = res
gap' [x] max res = res
gap' (x:y:xs) max res
    | (y - x) > max = gap' (y:xs) (y - x) (x,y)
    | otherwise = gap' (y:xs) max res

gap :: Int -> (Integer, Integer)
gap n = gap' [y | y <- take n primes, y < toInteger n] 0 (0,0)


-- Prvním argumentem je konec rozsahu, začátek bude vždy 2. Můžete předpokládat,
-- že konec bude alespoň 3.

-- 3) Implementujte mergesort, který vyhazuje duplikáty.

mergeWith :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeWith _ [] [] = []
mergeWith _ xs [] = xs
mergeWith _ [] ys = ys
mergeWith c (x:xs) (y:ys)
    | c x y == LT = x : mergeWith c xs (y:ys)
    | c x y == GT = y : mergeWith c (x:xs) ys
    | c x y == EQ = x : mergeWith c xs ys

sortWith  :: (a -> a -> Ordering) -> [a] -> [a]
sortWith _ [] = []
sortWith _ [x] = [x]
sortWith c xs = mergeWith c (sortWith c left) (sortWith c right)
    where (left, right) = splitAt (length xs `div` 2) xs

-- Prvním argumentem je funkce, která provádí porovnávání.
-- Ordering je datový typ, který obsahuje 3 konstanty: LT, EQ, GT
-- (less than, equal, greater than).
--
-- >>> sortWith compare [10,9..1]
-- [1,2,3,4,5,6,7,8,9,10]
--
-- >>> sortWith (flip compare) [10,9..1]
-- [10,9,8,7,6,5,4,3,2,1]
--
-- >>> sortWith compare [1,1,1]
-- [1]
--
-- BONUS)
--
-- Implementujte následující funkce:

-- combinations n x vygeneruje seznam všech kombinací délky n ze seznamu x.
-- Na pořadí kombinací ve výsledném seznamu nezáleží.
--
-- >>> combinations 2 "abcd"
-- ["ab","ac","ad","bc","bd","cd"]
--
combinations :: Int -> [a] -> [[a]]
combinations = undefined

-- permutations x vygeneruje seznam všech permutací. Na pořadí permutací ve
-- výsledném seznamu nezáleží.
--
-- >>> permutations "abc"
-- ["abc","bac","bca","acb","cab","cba"]
--
permutations :: [a] -> [[a]]
permutations = undefined

-- Pomocí těchto funkcí definujte "variace" (občas najdete v české literatuře,
-- v angličtině pro to termín asi neexistuje): kombinace, kde záleží na pořadí
--
-- >>> variations 2 "abc"
-- ["ab","ba","ac","ca","bc","cb"]
--
variations :: Int -> [a] -> [[a]]
variations = undefined
