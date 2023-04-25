-- 5. úloha
--
-- 1) Definujte datový typ 'Trie k v' reprezentující trii, kde klíče (řetězce)
-- jsou typu '[k]' a hodnoty typu 'v'.

------------------------------------------------------------------------------------------------

-- data Trie k v = Trie {value :: Maybe v, children :: [(k, Trie k v)]}
--     deriving (Show, Eq)


-- data Trie k v = Node (Maybe v) [(k, Trie k v)]
--     deriving (Show, Eq) -- ??? nvm xD

data Trie k v = ValueNode v [(k, Trie k v)] | Node [(k, Trie k v)]
    deriving (Show, Eq)

------------------------------------------------------------------------------------------------

-- Implementujte následující:

------------------------------------------------------------------------------------------------

-- empty :: Trie k v
-- empty = Node Nothing []

empty :: Trie k v
empty = Node []

------------------------------------------------------------------------------------------------


-- 'empty' je jednoduše konstanta, reprezentující prádznou trii.
--
-- > empty == fromList []
--

singleton :: [k] -> v -> Trie k v
singleton [] value = ValueNode value []
singleton (x:xs) value = Node [(x, singleton xs value)]

-- 'singleton ks v' je trie, která obsahuje právě jeden klíč 'ks'
-- s hodnotou 'v'.
--
-- > singleton ks v == fromList [(ks, v)]
--

insertWith :: (Ord k) => (v -> v -> v) -> [k] -> v -> Trie k v -> Trie k v
insertWith f [] new (Node children) = ValueNode new children
insertWith f [] new (ValueNode old children) = ValueNode (f old new) children

insertWith f (k:ks) new (Node children)
    |checkChild k children = Node (split f (k:ks) new children)
    |otherwise = Node ((k, insertWith f ks new (Node [])):children)

insertWith f (k:ks) new (ValueNode old children)
    |checkChild k children = ValueNode old (split f (k:ks) new children)
    |otherwise = ValueNode old ((k, insertWith f ks new (Node [])):children)

checkChild :: Eq t => t -> [(t, b)] -> Bool
checkChild _ [] = False
checkChild k ((key, _):children)
    | k == key = True
    | otherwise = checkChild k children

split :: Ord a => (t -> t -> t) -> [a] -> t -> [(a, Trie a t)] -> [(a, Trie a t)]
split f (k:ks) new [(key, ValueNode old children)]
    |k == key = [(key, insertWith f ks new (ValueNode old children))]
    |otherwise = [(key, ValueNode old children)]
split f (k:ks) new [(key, Node children)]
    |k == key = [(key, insertWith f ks new (Node children))]
    |otherwise = [(key, Node children)]
split f ks new (child:children) = split f ks new [child] ++ split f ks new children


-- insertWith f ks new (ValueNode old children) = ValueNode old (split f new ks children children)

-- insertWith f k:ks new (Node _ []) = insertWith f 

-- insertWith f k:ks new (Node _ ((key,node):cs))
--     | key == k = insertWith f ks new node
--     | otherwise insertWith f ks new cs 

-- 'insertWith f ks new t' vloží klíč 'ks' s hodnotou 'new' do trie 't'. Pokud
-- trie již klíč 'ks' (s hodnotou 'old') obsahuje, původní hodnota je nahrazena
-- hodnotou 'f new old'.
--
-- > insertWith (++) "a" "x" empty                  == fromList [("a","x")]
-- > insertWith (++) "a" "x" (fromList [("a","y")]) == fromList [("a","xy")]
--

insert :: (Ord k) => [k] -> v -> Trie k v -> Trie k v
insert = insertWith f
    where f new old = new

-- 'insert ks new t' vloží klíč 'ks' s hodnotou 'new' do trie 't'. Pokud trie
-- již klíč 'ks' obsahuje, původní hodnota je nahrazena hodnotou 'new'
--
-- Hint: použijte 'insertWith'
--
-- > insert "a" "x" (fromList [("a","y")]) == fromList [("a","x")]
--

find :: (Ord k) => [k] -> Trie k v -> Maybe v
find = undefined

-- 'find k t' vrátí hodnotu odpovídající klíči 'k' (jako 'Just v'), pokud
-- existuje, jinak 'Nothing'.
--
-- > find "a" empty                  == Nothing
-- > find "a" (fromList [("a","x")]) == Just "x"
--

member :: (Ord k) => [k] -> Trie k v -> Bool
member = undefined

-- 'member k t' zjistí, jestli se klíč 'k' nalézá v trii 't'.
--
-- Hint: použijte 'find'
--
--
-- Funkce 'fromList' není nutná, ale může se vám hodit pro testování.

fromList :: (Ord k) => [([k], v)] -> Trie k v
fromList = undefined

-- BONUS) Implementujte funkci

delete :: (Ord k) => [k] -> Trie k v -> Trie k v
delete = undefined

-- 'delete ks t' smaže klíč 'ks' (a odpovídající hodnotu) z trie 't', pokud
-- klíč 'ks' není v trii obsažený, 'delete' vrátí původní trii.
--
-- > delete "a" (fromList [("b","y")]) == fromList [("b","y")]
-- > delete "a" (fromList [("a","x")]) == fromList []
