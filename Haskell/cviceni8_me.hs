--mocnení funkcií
pow :: Int -> (a -> a) -> (a -> a)
pow 0 f x = x
pow n f x = f (pow (n-1) f x)

add :: Int -> Int -> Int
add x y = pow x succ y

mull :: Int -> Int -> Int
mull x y = pow x (add y) 0

power :: Int -> Int -> Int
power x y = pow y (mull x) 1

partitions :: [a] -> [([a],[a])]

substring :: String -> String -> String