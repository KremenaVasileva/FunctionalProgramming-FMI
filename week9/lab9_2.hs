saySign :: (Num a, Ord a) => a -> String
saySign x
    | x > 0 = "Positive"
    | x < 0 = "Negative"
    | otherwise = "0"


fibonacci :: (Integral a, Ord a) => a -> a
fibonacci n
    | n <= 0 = 0
    | n == 1 = 1
    | otherwise = fibonacci (n - 1) + fibonacci (n - 2)

countRoots :: (Num a, Ord a) => a -> a -> a -> String
countRoots a b c
    | a == 0 = "No roots"
    | d < 0 = "No real roots"
    | d == 0 = "One root"
    | otherwise = "Two roots"
    where d = (b^2) - (4 * a * c)

sayRoots :: (Floating a, Ord a) => a -> a -> a -> String
sayRoots a b c
    | a == 0 = "No roots"
    | d < 0 = "No real roots"
    | (x1 > 0) && (x2 > 0) = "Two positive roots"
    | (x1 < 0) && (x2 < 0) = "Two negative roots"
    | (x1 < 0) || (x2 < 0) = "One positive and one negative root"
    | otherwise = "A zero result"
    where d = (b^2) - (4 * a * c)
          x1 = (b + sqrt d) / (2 * a)
          x2 = (b - sqrt d) / (2 * a)

cylinderVolume :: (Floating a) => a -> a -> a
cylinderVolume r h = pi * (r ^ 2) * h

useless :: (Num a, Eq a) => a -> a -> a -> a -> a
useless _ _ _ 0 = 1
useless _ _ 0 _ = 1
useless _ 0 _ _ = 1
useless 0 _ _ _ = 1
useless a b c d = a + b + c + d

power :: (Num a, Integral b, Ord b) => a -> b -> a
power x n
    | n <= 0 = 1
    | n == 1 = x
    | n `mod` 2 == 0 = power (x * x) (div n 2)
    | otherwise = x * power x (n - 1)