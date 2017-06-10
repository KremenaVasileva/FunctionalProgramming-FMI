-- Зад.1. Да се напише функцията sumProducts, която приема списък от списъци от числа и връща 
-- сумата на всички произведения на числата в даден списък:
-- sumProducts [[1,2,3], [4,5], [], [-2,3,0,5,1]] -> 6 + 20 + 0 + 0 = 26
sumProducts :: [[Int]] -> Int
sumProducts lst = foldr (+) 0 (products lst)
    where products :: [[Int]] -> [Int]
          products lst'
              | null lst' = []
              | null (head lst') = 0 : (products (tail lst'))
              | otherwise = foldr (*) 1 (head lst') : (products (tail lst'))

-- Зад.2. Да се напише функцията occurrences, която приема два списъка от числа и връща 
-- списък от броя срещания на елементите от първия списък във втория:
-- occurrences [1..6] [1,3,4,3,2,3,3,0,5,3,1] -> [2,1,5,1,1,0]
occurrences :: [Int] -> [Int] -> [Int]
occurrences lst1 lst2 = [ occurrence x | x <- lst1]
    where occurrence x = length [ num | num <- lst2, num == x]

-- Зад.3. Да се напише функцията mainDiag, която намира главния диагонал на квадратна матрица от числа:
-- mainDiag [[1,2,3],[4,5,6],[7,8,9]] -> [1,5,9]

mainDiag :: [[Int]] -> [Int]
mainDiag lst
    | null lst = []
    | otherwise = (head (head lst)) : (mainDiag (map tail (tail lst)))

-- Зад.4. Да се напише функция isSquare, която проверява дали дадена матрица е квадратна:
-- isSquare [[1,2,3],[4,5,6],[7,8,9]] -> True
-- isSquare [[1..4],[5..8],[9..12]] -> False

allEqual :: [Int] -> Bool
allEqual lst
  | null lst = True
  | null (tail lst) = True
  | (head lst) == (head (tail lst)) = allEqual (tail lst)
  | otherwise = False

isSquare :: [[Int]] -> Bool
isSquare lst = allEqual (length lst : map length lst)

-- Зад.5. Да се напише функцията sndDiag, която намира вторичния диагонал на квадратна матрица от числа:
-- sndDiag [[1,2,3],[4,5,6],[7,8,9]] -> [3,5,7]
sndDiag :: [[Int]] -> [Int]
sndDiag lst = mainDiag (map reverse lst)

-- Зад.6. Да се напише функцията matchLengths, която проверява дали всички списъци от списък със списъци са с еднаква дължина:
-- matchLengths [[1..4],[0..3],[5,4,8,10]] -> True
-- matchLengths [[1..4],[0..3],[],[5,4,8,10]] -> False
matchLengths :: [[a]] -> Bool
matchLengths lst = allEqual (map length lst)

-- Зад.7. За тази задача ще представяме множествата като наредени списъци от своите елементи 
-- (значи те трябва да могат да бъдат сравнявани с == и < и да се срещат точно по веднъж). 
-- Да се напишат функции setUnion, setIntersect и setDiff, които намират съответно обединението, 
-- сечението и разликата на две множества:
-- setUnion [1,2,3,5] [2,4,5,6,7] -> [1,2,3,4,5,6,7]
-- setIntersect [1,2,3,5] [2,4,5,6,7] -> [2,5]
-- setDiff [1,2,3,5] [2,4,5,6,7] -> [1,3]
-- setDiff [2,4,5,6,7] [1,2,3,5] -> [4,6,7]

setUnion :: (Eq a, Ord a) => [a] -> [a] -> [a]
setUnion [] lst = lst
setUnion lst [] = lst
setUnion (x:xs) (y:ys)
    | x < y = x : (setUnion xs (y:ys))
    | x == y = x : (setUnion xs ys)
    | x > y = y : (setUnion (x:xs) ys)

setIntersect :: (Eq a, Ord a) => [a] -> [a] -> [a]
setIntersect [] lst = []
setIntersect lst [] = []
setIntersect (x:xs) (y:ys)
    | x < y = setIntersect xs (y:ys)
    | x == y = x : (setIntersect xs ys)
    | x > y = setIntersect (x:xs) ys

setDiff :: (Eq a, Ord a) => [a] -> [a] -> [a]
setDiff [] lst = []
setDiff lst [] = lst
setDiff (x:xs) (y:ys)
    | x < y = x : (setDiff xs (y:ys))
    | x == y = setDiff xs ys
    | x > y = setDiff (x:xs) ys

-- Зад.8. Да се напише функция specialSort, която приема като параметър списък от списъци 
-- и го сортира относно най-често срещания елемент във всеки от вътрешните списъци. 
-- Ако има няколко най-често срещани елемента, да се избира най-големия от тях:
-- specialSort ["moo", "bee", "eve", "abracadabra", "abcdefg", "mama", "z"]
--   -> ["abracadabra", "bee", "eve", "abcdefg", "mama", "moo", "z"]
-- в случая най-често срещаните елементи са 'a', 'e', 'e', 'g', 'm', 'o', 'z'
-- наредбата не е уникална - няма проблем с това, нямаме изисквания за стабилност

-- specialSort :: (Eq a, Ord a) => [[a]] -> [[a]]

data Car = Car { company :: String  
               , model :: String  
               , year :: Int  
               } deriving (Show)