-- 1.
hailstone :: Int -> [Int]
hailstone 1 = [1]
hailstone n
    | mod n 2 == 0 = n : (hailstone (div n 2))
    | otherwise = n : (hailstone ((3 * n) + 1))

-- 2.
-- all the squares which we may need
squares = [ x*x | x <- [1 .. 7]]

-- predicate function finding whether an Int is prime or not
prime :: Int -> Bool
prime 1 = False
prime n = null [ divs | divs <- [2 .. (n-1)], mod n divs == 0]

-- all the prime numbers we may need
primes = [ x | x <- [2 .. 100], prime x]

-- a function which takes a list of Ints and returns a list with no duplicates
makeSet :: [Int] -> [Int]
makeSet [] = []
makeSet (x:xs) = x : makeSet (filter (/= x) xs)

result = length (makeSet [ x | x <- [10 .. 99], odd x, not (elem x primes), y <- primes, sq <- squares, not (x == (y + 2 * sq))])

-- 3.
allPrimeDivisors x = filter prime [ divs | divs <- [2 .. x], mod x divs == 0]

-- finds how many times a number divides another number
timesDividing x divisor
    | mod x divisor == 0 = 1 + (timesDividing (div x divisor) divisor)
    | otherwise = 0

divisors :: Int -> [(Int, Int)]
divisors n = [(divisor, timesDividing n divisor) | divisor <- allPrimeDivisors n]


-- 4.
intercalate' :: String -> [String] -> String
intercalate' str [] = []
intercalate' str (x:[]) = x
intercalate' str (x:xs) = x ++ str ++ intercalate' str xs