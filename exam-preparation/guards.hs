-- let example
fact2 :: Int -> Int
fact2 n = let fact n = if n == 0 then 1 
                       else n * fact (n - 1)
          in (fact n) ^ 2

-- sums the last 2 digits of the number n
sumLastDigits :: Int -> Int
sumLastDigits n = lastDigit n + lastDigit (stripDigit n)
    where lastDigit = (`mod` 10)
          stripDigit = (`div` 10)

-- the (++) function for concatenating 2 lists
(++*) :: [a] -> [a] -> [a]
(++*) lst1 lst2 = if null lst1 then lst2
                              else head lst1 : (tail lst1 ++ lst2)

-- the reverse function
reverse' :: [a] -> [a]
reverse' lst
    | null lst = []
    | otherwise = reverse' (tail lst) ++ [head lst]

-- the (!!) function
(!!*) :: [a] -> Int -> a
(!!*) lst n
    | null lst = error "index too large"
    | n == 0 = head lst
    | otherwise = (!!*) (tail lst) (n - 1)

-- the map function
map' :: (a -> b) -> [a] -> [b]
map' f lst = [ f x | x <- lst]

-- the filter function
filter' :: (a -> Bool) -> [a] -> [a]
filter' p lst = [ x | x <- lst, p x]

-- the foldr function
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' op nv [] = nv
foldr' op nv (x:xs) = op x (foldr' op nv xs)

-- the foldl function
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' op nv [] = nv
foldl' op nv (x:xs) = foldl' op (op nv x) xs

-- the foldr1 function
foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' op [x] = x
foldr1' op (x:xs) = op x (foldr1 op xs)

-- the foldl1 function
foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' op (x:y:xs) = foldl1' op (op x y : xs)
foldl1' op (x:xs) = x

-- the repeat function
repeat' :: a -> [a]
repeat' x = x : repeat' x

-- the cycle function
cycle' :: [a] -> [a]
cycle' lst = lst ++ cycle' lst

-- the iterate function
iterate' :: (a -> a) -> a -> [a]
iterate' f a = a : iterate' f (f a)