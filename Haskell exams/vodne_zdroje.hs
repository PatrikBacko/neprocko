-- pôvodná mapa
type Mapa_ = [((Int, Int), [(Int, Int)])]
-- b)
type Mapa a = [((a, a), [(a, a)])]

-- a)
zdroje :: Ord a => Mapa a -> [((a, a), (a, a))]
zdroje [] = []
zdroje rs = concatMap pripojZdroj rs
    where 
        pripojZdroj (_, []) = []
        pripojZdroj ((x, y), zs) = [((x, y), n)]
            where 
                n = head $ filter (\(a,b) -> (a <= x) && (b <= y)) zs

-- d) 
-- zdroje :: Ord a -> Mapa a -> (a -> a -> bool) -> [((a, a), (a, a))]

-- skompiluje sa v pohode
-- neviem či funguje tak ako má, nemám test data zo skúšky
-- skúšal som nejakých pár vlastných testov
-- a myslím si, že to robí čo má, ale nvm :/

-- testy
-- zdroje [((0,0), []), ((1,0), [(0,0)]), ((1,1), [(0,0), (1,0)])]
-- zdroje [((0,0), []), ((1,0), [(0,0)]), ((1,1), [(0,0), (1,0)]), ((2,2), [(1,1), (1,0)]), ((0,1), [(1,1), (0,0), (2,2)])]

