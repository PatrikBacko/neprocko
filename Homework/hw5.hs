import Data.Maybe (isNothing)
-- 5. úloha
--
-- 1) Definujte datový typ 'Trie k v' reprezentující trii, kde klíče (řetězce)
-- jsou typu '[k]' a hodnoty typu 'v'.

data Trie k v = ValueNode v [(k, Trie k v)] | Node [(k, Trie k v)]
    deriving (Show, Eq)

-- Implementujte následující:

empty :: Trie k v
empty = Node []

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
insertWith f [] new (ValueNode old children) = ValueNode (f new old) children

insertWith f (k:ks) new (Node children) = Node ((key, insertWith f ks new child):other)
    where   ((key, child):other) = processChildren k children []
    
insertWith f (k:ks) new (ValueNode old children) = ValueNode old ((key, insertWith f ks new child):other)
    where   ((key, child):other) = processChildren k children []

processChildren :: (Ord k) => k -> [(k, Trie k v)] -> [(k, Trie k v)] -> [(k, Trie k v)]
processChildren k [] acc = (k, Node []):acc
processChildren k ((key, node):children) acc
    | k == key = (key, node):(children ++ acc)
    | otherwise = processChildren k children ((key, node):acc)

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

find' :: (Ord k) => [k] -> [(k, Trie k v)] -> Maybe v
find' _ [] = Nothing 
find' (k:ks) ((key, node):children)
    | k == key = find ks node
    | otherwise = find' (k:ks) children

find :: (Ord k) => [k] -> Trie k v -> Maybe v
find [] (Node _) = Nothing
find [] (ValueNode value _) = Just value
find (k:ks) (Node children) = find' (k:ks) children
find (k:ks) (ValueNode _ children) = find' (k:ks) children

-- 'find k t' vrátí hodnotu odpovídající klíči 'k' (jako 'Just v'), pokud
-- existuje, jinak 'Nothing'.
--
-- > find "a" empty                  == Nothing
-- > find "a" (fromList [("a","x")]) == Just "x"
--

member :: (Ord k) => [k] -> Trie k v -> Bool
member k t
    | isNothing(find k t) = False
    | otherwise = True

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
