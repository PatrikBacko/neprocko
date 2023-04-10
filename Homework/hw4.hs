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

-- >>> take 5 primes
-- [2,3,5,7,11]

--sieve (x:xs) = x : sieve[y| y <- xs, y `mod` x /= 0]

-- primes :: [Integer]
-- primes = sieve [2..]

-- 3) Implementujte mergesort.


-- mergeWith :: (a -> a -> Bool) -> [a] -> [a] -> [a]
-- mergeWith _ xs [] = xs
-- mergeWith _ [] ys = ys
-- mergeWith comparer (x:xs) (y:ys)
--     | x comparer y = y : mergeWith comparer (x:xs) ys
--     | otherwise = x : mergeWith comparer xs (y:ys)

-- sortWith  :: (a -> a -> Bool) -> [a] -> [a]
-- sortWith c x = sortWith c ([]) : sortWith c ()

-- Prvním argumentem je funkce, která provádí porovnávání.
--
-- >>> sortWith (<) [10,9..1]
-- [1,2,3,4,5,6,7,8,9,10]
--
-- >>> sortWith (>) [10,9..1]
-- [10,9,8,7,6,5,4,3,2,1]
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
