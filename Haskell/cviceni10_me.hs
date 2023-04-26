data RoseTree a = Node a [RoseTree a]

show' :: Show a => String -> RoseTree a -> String
show' ys (Node x ts) = ys ++ show x ++ "\n" ++ concatMap (show' (ys ++ "    ")) ts 

instance Show a => Show (RoseTree a) where 
    show :: Show a => RoseTree a -> String
    show (Node x ts) = show' "" (Node x ts)
    -- show (Node x ts) = show x ++ "\n" ++ addEach (concatMap show ts)
    --      where
    --          addEach = unlines . map ("    "++) . lines

instance Functor RoseTree where
    fmap :: (a -> b) -> RoseTree a -> RoseTree b
    fmap f (Node x ts) = Node (f x) (map (fmap f) ts)
