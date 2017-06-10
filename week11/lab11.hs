-- Зад.1. Да се напише функция zipWith', която по дадена двуаргументна функция и два списъка връща списък, 
-- получен от прилагането на функцията върху съответните елементи на списъците:
-- zipWith' (+) [1..5] [0.3, 0.4, 0.6] -> [1.3, 2.4, 3.6]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y) : (zipWith' f xs ys)

-- Зад.2. Да се напише функция flip', която разменя аргументите на дадена двуаргументна функция
-- и връща новополучената функция:
-- (flip' div) 5 10 -> 2

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' fn = (\x y -> fn y x)

-- Зад.3. Да се напише функция takeWhile', която приема предикат и списък и връща 
-- тези елементи от началото на списъка, които изпълняват предиката:
-- takeWhile' even [2,4,5,6,7,8] -> [2,4] -- забележете - функцията "спира" при първото срещнато нечетно число и не връща 6 или 8!

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' fn [] = []
takeWhile' fn (x:xs)
    | fn x = x : (takeWhile' fn xs)
    | otherwise = []

-- Зад.4. Да се напише функция compress, която по списък от стойности връща 
-- списък от наредени двойки от вида (<стойност>, <брой последователни срещания>):
-- compress [1,1,2,3,3,3,4,2,2,2,1,1] -> [(1,2),(2,1),(3,3),(4,1),(2,3),(1,2)]
-- compress "abba" -> [('a',1),('b',2),('a',1)]

-- histogram
compress' :: (Eq a) => [a] -> [(a, Int)]
compress' [] = []
compress' lst@(x:xs) = (x, numberOf x) : (compress' (removeAll x))
    where numberOf x = length [y | y <- lst, y == x]
          removeAll x = [y | y <- lst, y /= x]

compress2 :: (Eq a) => [a] -> [(a, Int)]
compress2 [] = []
compress2 lst@(x:xs) = (x, numberOf x) : (compress2 (remove x))
    where numberOf x = length (takeWhile' (\y -> y == x) lst)
          remove x = (drop (numberOf x) lst)

-- Зад.5. Да се напише функция maxRepeated, която по списък от стойности връща 
-- дължината на най-дългия подсписък, съставен от еднакви стойности:
-- maxRepeated [1,1,2,3,3,3,4,2,2,2,1,1] -> 3

maxRepeated :: (Eq a, Ord a) => [a] -> Int
maxRepeated lst = maximum [ x | x <- occurrences]
                    where occurrences = map snd (compress2 lst)

-- Зад.6. Да се напише функция makeSet, която по даден списък връща всички негови уникални елементи (редът им няма значение):
-- makeSet [1,1,2,3,3,3,4,2,2,2,1,1] -> [1,2,3,4]
-- makeSet "abba" -> "ab"

makeSet :: (Eq a) => [a] -> [a]
makeSet lst = map fst (compress' lst)

-- Зад.7. Да се напише функция histogram, която по списък от стойности връща 
-- списък от наредени двойки от видя (<стойност>, <общ брой срещания>):
-- histogram [1,1,2,3,3,3,4,2,2,2,1,1] -> [(1,4),(2,4),(3,3),(4,1)] == compress'

-- Зад.8. Да се напише функция maxDistance, която получава списък от точки (наредени двойки (Double, Double)) и връща 
-- дължината на най-дългата отсечка между някои две от тях.
-- maxDistance [(-1.1, 1), (1.8, 2), (3, 1), (-1, -2)] -> 5

maxDistance :: [(Double, Double)] -> Double
maxDistance lst = maximum [findDistance x y | x <- lst, y <- lst]
                     where findDistance (x1, y1) (x2, y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)


-- Зад.9. Да се напише функция compositions, която по дадена едноаргументна функция f 
-- съставя списък от всички ѝ композиции (започвайки от самата f)
compositions :: (a -> a) -> [(a -> a)]
compositions f = [ composeN f i | i<-[1..] ]
  where composeN :: (a -> a) -> Int -> (a -> a)
        composeN f 1 = f
        composeN f n = f . (composeN f (n-1))