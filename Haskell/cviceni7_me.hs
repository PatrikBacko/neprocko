pyth_1 a b = sqrt(a*a + b*b)


triples n = [(x,y,z) | x <- [1,2..n], y <- [1,2..n], z <- [1,2..n] ]


avg :: [Double] -> Double
avg a = (sum a) / fromIntegral ((length a))