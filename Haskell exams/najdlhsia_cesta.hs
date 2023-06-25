import Data.Maybe

type Graf_ = [(String , [(String, Int)])]
type Graf a b = [(a , [(a, b)])]


g :: Graf String Int
g = [   ("a", [("b",1), ("c", 1), ("f", 3)]),
        ("b", [("d", 2)]),
        ("c", [("b", 2), ("e", 1)]),
        ("d", [("c", 2)]),
        ("e", [("d", 3)]),
        ("f", [("c", 4), ("e", 2)])
    ]

-- nc :: Graf -> String -> String -> Maybe ([String], Int)
nc :: (Num n, Eq a, Ord n) => [(a, [(a, n)])] -> a -> a -> Maybe ([a], n)
nc g u v 
    | result == [] = Nothing
    | otherwise = Just (reverse $ fromJust $ lookup m result, m)
        where 
            result = map (\(a,n) -> (n,a)) (findroutes g u v [] 0)
            m = maximum (map fst result)

findroutes :: (Num n, Eq a) => [(a, [(a, n)])] -> a -> a -> [a] -> n -> [([a], n)]
findroutes g u v c l = concatMap (route gNew v (u:c) l) ns
    where 
        ns = findNeighbours g u
        gNew = filter (\(a,b) -> a /= u) g 


route :: (Num n, Eq a) => [(a, [(a, n)])] -> a -> [a] -> n -> (a, n) -> [([a], n)]
route g v c l (u,i) 
    | v == u = [(u:c, l+i)]
    | otherwise = findroutes g u v c (l+i)

findNeighbours :: Eq a => [(a, [b])] -> a -> [b]
findNeighbours g u
    | isNothing ns = []
    | otherwise = fromJust ns
    where 
        ns = lookup u g

-- d) išlo to spraviť bez zmeny kódu, stačilo mi zobecniť signatúry