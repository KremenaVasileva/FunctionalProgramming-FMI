modulus :: (Floating a) => a -> a -> a
modulus a b = sqrt ((a ^ 2) + (b ^ 2))

complAdd :: (Floating a) => (a, a) -> (a, a) -> (a, a)
complAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

complSub :: (Floating a) => (a, a) -> (a, a) -> (a, a)
complSub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

complMul :: (Floating a) => (a, a) -> (a, a) -> (a, a)
complMul (x1, y1) (x2, y2) = (x1 * x2 - y1 * y2, x1 * y2 + x2 * y1)

ackermann :: (Integral a, Ord a) => a -> a -> a
ackermann m n
    | m == 0 = n + 1
    | (m > 0) && (n == 0) = ackermann (m - 1) 1
    | (m > 0) && (n > 0) = ackermann (m - 1) (ackermann m (n - 1))

distance :: (Floating a) => (a, a) -> (a, a) -> a
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

replicate' :: (Integral a, Ord a) => a -> b -> [b]
replicate' n x
    | n <= 0 = []
    | otherwise = x:(replicate' (n - 1) x)

take' :: (Integral a, Ord a) => a -> [b] -> [b]
take' _ [] = []
take' 0 _ = []
take' n (x:xs) = x : (take' (n - 1) xs)

drop' :: (Integral a, Ord a) => a -> [b] -> [b]
drop' _ [] = []
drop' 0 lst = lst
drop' n (x:xs) = drop' (n - 1) xs

prime :: Integral a => a -> Bool
prime 1 = False
prime n = null [d | d <- [2 .. (n - 1)], n `mod` d == 0]


primes = [x | x <- [1 .. ], prime x]

nthPrime :: Int -> Int
nthPrime n = primes !! (n - 1)

removeAt :: Int -> [b] -> [b]
removeAt _ [] = []
removeAt 0 (x:xs) = xs
removeAt n (x:xs) = x : (removeAt (n - 1) xs)

removeNth :: (Integral a, Ord a) => a -> [b] -> [b]
removeNth _ [] = []
removeNth n lst = (take' (n - 1) lst) ++ (removeNth n (drop' n lst))

-- 11. Генерирайте безкрайния списък на простите числа по метода на ситото на Ератостен (използвайки предишната функция).
primes' :: [Int]
primes' = sieve [2..]
    where sieve (x:xs) = x : (removeNth x xs)