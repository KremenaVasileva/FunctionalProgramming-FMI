-- removes every nth element of a list:
-- removeNth 3 [1..10] -> [1,2,4,5,7,8,10]     -- missing 3, 6, 9
removeNth :: Int -> [Int] -> [Int]
removeNth _ [] = []
removeNth n lst = take (n - 1) lst ++ (removeNth n (drop n lst))

-- Generating the infinite prime list using the sieve of Eratosthenes (and the function above):
allPrimes = sieve [2 .. ]
            where sieve (x:xs) = x : removeNth x xs

-- _________________________________
-- lab 11
-- 1. function zipWith' which takes 3 arguments: one two-argument function (FN) and two lists (list1 and list2)
-- and returns a list with elements (list1 FN list2):
-- zipWith' (+) [1..5] [0.3, 0.4, 0.6] -> [1.3, 2.4, 3.6]
zipWith' :: (a -> a -> b) -> [a] -> [a] -> [b]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- 2. function flip' which flips the arguments of a given two-argument function and returns the new function:
-- (flip' div) 5 10 -> 2
flip' :: (a -> a -> b) -> a -> a -> b
flip' f x y = f y x

-- 3. function takeWhile' which takes two arguments: a predicate and a list, and returns those elements
-- from the beginning of the list which satisfy the predicate:
-- takeWhile' even [2,4,5,6,7,8] -> [2,4] -- note: the function stops right after it finds an odd number and DOES NOT return 6 or 8
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs)
    | p x = x : takeWhile' p xs
    | otherwise = []

-- 4. function compress, which takes one argument: a list of tuples, and returns a list of tuples: 
-- (<value>, <number of consecutive occurrences>):
-- compress [1,1,2,3,3,3,4,2,2,2,1,1] -> [(1,2),(2,1),(3,3),(4,1),(2,3),(1,1)]
-- compress "abba" -> [('a',1),('b',2),('a',1)]
compress :: (Eq a) => [a] -> [(a, Int)]
compress [] = []
compress lst = [ (head x, length x) | x <- group lst]

group :: (Eq a) => [a] -> [[a]]
group [] = []
group lst@(x:xs) = takeWhile' (==x) lst : group (dropWhile (==x) lst)

-- 5. function maxRepeated, which takes one argument: a list, and returns the length of the longest sublist with equal elements:
-- maxRepeated [1,1,2,3,3,3,4,2,2,2,1,1] -> 3
maxRepeated :: (Eq a) => [a] -> Int
maxRepeated lst = maximum . map snd . compress $ lst

-- 6. function makeSet, which takes one argument: a list 
-- and returns all its unique elements (their order doesn't matter):
-- makeSet [1,1,2,3,3,3,4,2,2,2,1,1] -> [1,2,3,4]
-- makeSet "abba" -> "ab"
makeSet :: (Eq a) => [a] -> [a]
makeSet [] = []
makeSet (x:xs) = x : makeSet (remove x xs)

remove :: (Eq a) => a -> [a] -> [a]
remove _ [] = []
remove k (x:xs)
    | k == x = remove k xs
    | otherwise = x : remove k xs

-- 7. function histogram, which takes one argument: a list
-- and returns a list of tuples: (<value>, <number of occurrences>):
-- histogram [1,1,2,3,3,3,4,2,2,2,1,1] -> [(1,4),(2,4),(3,3),(4,1)]
histogram :: (Eq a) => [a] -> [(a, Int)]
histogram [] = []
histogram lst = zip set (map (`occurNum` lst) set)
                where set = makeSet lst

occurNum :: (Eq a) => a -> [a] -> Int
occurNum y [] = 0
occurNum y (x:xs)
    | x == y = 1 + occurNum y xs
    |otherwise = occurNum y xs

-- 8. function maxDistance, which takes one argumet: a list of points (tuples (Double, Double))
-- and returns the length of the longest distance between any two of them:
-- maxDistance [(-1.1, 1), (1.8, 2), (3, 1), (-1, -2)] -> 5
maxDistance :: [(Double, Double)] -> Double
maxDistance [] = 0
maxDistance lst = maximum allDistances
                    where allDistances = [ dist x y | x <- lst, y <- lst, not (x == y)]
                          dist (x1, y1) (x2, y2) = sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2)

-- ______________________________________________________________________
-- lab 12
-- 1. function sumProducts, which takes one argument: a list of lists of Int
-- and returns the sum of the products of all the elements in each sublist:
-- sumProducts [[1,2,3], [4,5], [], [-2,3,0,5,1]] -> 27 -- 27 = 6 + 20 + 1 + 0
sumProducts :: [[Int]] -> Int
sumProducts [] = 0
sumProducts lst = foldr (+) 0 products
                     where products = map product lst

-- 2. function occurrences, which takes two arguments: two lists
-- and returns a list with the number of occurrences of each element from the first list in the second list:
-- occurrences [1..6] [1,3,4,3,2,3,3,0,5,3,1] -> [2,1,5,1,1,0]
occurrences :: (Eq a) => [a] -> [a] -> [Int]
occurrences set lst = map (`occurNum` lst) set

-- 8. function specialSort, which takes one argument: a list of lists
-- and sorts it according the most common element in each of the sublists.
-- If there are a few most common elements, the "biggest" one should be chosen:
-- specialSort ["moo", "bee", "eve", "abracadabra", "abcdefg", "mama", "z"]
--  -> ["abracadabra", "bee", "eve", "abcdefg", "mama", "moo", "z"]
--  In the example the most common elements are: 'a', 'e', 'e', 'g', 'm', 'o', 'z
-- specialSort :: (Eq a, Ord a) => [[a]] -> [[a]]

-- map head (map (\x -> quicksort' (\pair1 pair2 -> snd pair1 > snd pair2) x) (map histogram ["abracadabra", "bee", "eve", "abcdefg", "mama", "moo", "z"]))
-- map (\x -> [ x, histogram x]) ["abracadabra", "bee", "eve", "abcdefg", "mama", "moo", "z"]
-- ______________________________________________________________________
-- lab 13
-- 1. Let: type Point = (Double, Double) be a predefined type for points in the plane with real coordinates.
-- Code a function maxDistance, which finds the max Euclidean distance between any two points from a list:
-- maxDistance [(-1.1, 1), (1.8, 2), (3, 1), (-1, -2)] -> 5
type Point = (Double, Double)

maxDistance' :: [Point] -> Double
maxDistance' [] = 0
maxDistance' lst = maximum allDistances
                    where allDistances = [ dist p1 p2 | p1 <- lst, p2 <- lst, not (p1 == p2)]
                          dist p1 p2 = sqrt ((fst p2 - fst p1) ** 2 + (snd p2 - snd p1) ** 2)

-- 2. Let: type Item = (String, Integer) be a predefined type for an item and its expiration date (in days).
-- If the expiration date is a negative number x, that would mean that the item has expired |x| days ago.
-- Code a function: expiringItems :: [Item] → (String, Integer, String), 
-- which takes a list of Items and returns: (<the_item_with_the_shortest_exp_date>, <the_number_of_the_already_expired_items>, 
-- <the_item_which_has_expired_first>): 
-- expiringItems [("Milk",3), ("Bread",1), ("Yoghurt",-3), ("Butter",5), ("Cheese",-1), ("Pasta",2)] → ("Bread", 2, "Yoghurt")
-- two expired items: "Yoghurt" and "Cheese", and "Yoghurt" is the first which has expired.
type Item = (String, Integer)

quicksort :: (Eq a, Ord a) => [a] -> [a]
quicksort [] = []
quicksort [x] = [x]
quicksort (pivot:rest) = quicksort [ x | x <- rest, x < pivot]
                         ++ [pivot] ++ [ x | x <- rest, x == pivot]
                         ++ quicksort [ x | x <- rest, x > pivot]

quicksort' :: (a -> a -> Bool) -> [a] -> [a]
quicksort' _ [] = []
quicksort' _ [x] = [x]
quicksort' comp lst@(pivot:rest) = quicksort' comp [ x | x <- rest, comp x pivot]
                                    ++ [pivot] ++ quicksort' comp [ x | x <- rest, not (comp x pivot), not (comp pivot x)]
                                    ++ quicksort' comp [ x | x <- rest, comp pivot x]

expiringItems :: [Item] -> (String, Int, String)
expiringItems items = (firstExpiring, numExpired, firstExpired)
                      where firstExpiring = fst . head . filter (\item -> snd item > 0) $ sortedItems
                            numExpired = length . filter (\item -> snd item < 0) $ sortedItems
                            firstExpired = fst . head $ sortedItems
                            sortedItems = quicksort' (\item1 item2 -> snd item1 < snd item2) items