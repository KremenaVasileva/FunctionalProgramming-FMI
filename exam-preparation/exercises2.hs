-- lab 14

-- making new data type IntTree (a binary tree with int nodes)
data IntTree = Empty | Node Int IntTree IntTree deriving Show

-- making new data type VarTree (a tree with random children and int nodes)
data VarTree = Empty' | Node' Int [VarTree]

vt :: VarTree
vt = Node' 5 [(Node' 2 []), Empty', (Node' 3 []), (Node' 4 []), (Node' 5 [])]

-- най-често при работата с такова дърво map-ваме нещо над всички
-- наследници на възела, и акумулираме получения списък със стойности
treeSum' :: VarTree -> Int
treeSum' Empty' = 0
treeSum' (Node' x children) = x + sum (map treeSum' children)

-- Втори вариант за представяне: 
data VarTree2 = Leaf Int | VNode Int [VarTree2]

vt2 :: VarTree2
vt2 = VNode 5 [Leaf 2, Leaf 3, Leaf 4, Leaf 5]
-- стандартни рекурсивни функции
-- много по-лесно се пишат и разбират, когато се
-- възползваме от pattern matching за конструкторите на дървото
-- в случая Empty символизира празното дърво, но на практика е конструктор (без аргументи)
treeSize :: IntTree -> Int
treeSize Empty = 0
treeSize (Node val left right) = 1 + treeSize left + treeSize right

treeSum :: IntTree -> Int
treeSum Empty = 0
treeSum (Node val left right) = val + treeSum left + treeSum right

isEmpty :: IntTree -> Bool
isEmpty Empty = True
isEmpty _ = False

makeLeaf :: Int -> IntTree
makeLeaf x = Node x Empty Empty

testT1 :: IntTree
testT1 = Node 5 (Node 3 Empty
                        (makeLeaf 1))
                (makeLeaf 2)
-- _______________________________________
-- Зад.3. Да се напише функция maxSumPath, която приема за аргумент двоично дърво с числа във възлите 
-- и намира максималната сума на числата по някой път от корен до листо.
maxSumPath :: IntTree -> Int
maxSumPath Empty = 0
maxSumPath (Node val left right) = val + max (maxSumPath left) (maxSumPath right)

-- Зад.4. Да се напише функция prune, която по дадено двоично дърво t връща ново дърво t', 
-- което представлява t, в което всички листа са премахнати.
prune :: IntTree -> IntTree
prune Empty = Empty
prune (Node _ Empty Empty) = Empty
prune (Node val left right) = (Node val (prune left) (prune right))

-- Зад.5. Да се напише функция bloom, която по дадено двоично дърво t връща ново дърво t', 
-- което представлява t, в което на всички листа са добавени по два наследника - нови листа. 
-- Стойността в тези нови листа да е същата, като в оригиналното листо, от което са излезли.
bloom :: IntTree -> IntTree
bloom Empty = Empty
bloom (Node val Empty Empty) = (Node val (Node val Empty Empty) (Node val Empty Empty))
bloom (Node val left right) = (Node val (bloom left) (bloom right))

-- Зад.6. Да се дефинира тип BST, който да представлява двоично наредено дърво, 
-- съдържащо стойности от произволен тип във възлите си. 
-- Да се дефинират следните функции към него:
-- - bstinsert :: (Eq a, Ord a) => a -> BST a -> BST a - добавяне на стойност в дървото
-- - bstsearch :: (Eq a, Ord a) => a -> BST a -> Bool  - търсене на стойност в дървото
-- - bstvalues :: BST a -> [a]                         - получаване на списък със всички стойности в дървото
-- - bstsize :: BST a -> Integer                      - брой стойности, съдържани в дървото

-- Дърво, което съдържа произволни стойности по върховете - 
-- приема съдържания тип като параметър подобно на C++.
-- Оттам нататък винаги дървото върви с типа си - не пишем BST,
-- както на пишем std::vector, ами "BST a", "BST Int" като
-- "std::vector<T>" и "std::vector<int>".
data BST a = EmptyBST | BSTNode a (BST a) (BST a) deriving Show
bstvalues :: BST a -> [a]
bstvalues EmptyBST = []
bstvalues (BSTNode a left right) = [a] ++ bstvalues left ++ bstvalues right

bstsize :: BST a -> Int
bstsize EmptyBST = 0
bstsize (BSTNode _ left right) = 1 + bstsize left + bstsize right

bstinsert :: (Eq a, Ord a) => a -> BST a -> BST a
bstinsert val EmptyBST = (BSTNode val EmptyBST EmptyBST)
bstinsert val (BSTNode x left right)
    | val <= x = (BSTNode x (bstinsert val left) right)
    | otherwise = (BSTNode x left (bstinsert val right))

bstsearch :: (Eq a, Ord a) => a -> BST a -> Bool
bstsearch _ EmptyBST = False
bstsearch val (BSTNode x left right)
    | val == x = True
    | val < x = bstsearch val left
    | otherwise = bstsearch val right

bstFromList :: (Eq a, Ord a) => [a] -> BST a
bstFromList = foldr bstinsert EmptyBST
-- bstFromList lst = foldr bstinsert EmptyBST lst

testBST :: BST Int
testBST = bstFromList [5,3,6,2,4]

-- Зад.7. Да се дефинира тип Map, който да представлява структурата от данни map, 
-- реализирана с двоично наредено дърво. Да се дефинират следните функции към нея:
-- - mapinsert :: (Eq k, Ord k) => k -> v -> Map k v -> Map k v - вмъкване на ключ със стойност в дървото. 
-- 				Ако стойност за този ключ съществува, нека тя да бъде заместена с новата.
-- - mapsearch :: (Eq k, Ord k) => k -> Map k v -> Maybe v -- (!) търсене на стойност по ключ в дървото (обърнете внимание на върнатия тип)
data Map k v = EmptyMap | MapNode (k, v) (Map k v) (Map k v) deriving Show
mapinsert :: (Eq k, Ord k) => k -> v -> Map k v -> Map k v
mapinsert key val EmptyMap = MapNode (key, val) EmptyMap EmptyMap
mapinsert key val (MapNode (k, v) left right)
    | key == k = (MapNode (k, val) left right)
    | key < k = (MapNode (k, v) (mapinsert key val left) right)
    | otherwise = (MapNode (k, v) left (mapinsert key val right))