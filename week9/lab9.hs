-- GHCI
-- cd ./random-dir
-- load "<file-name>"
-- :r // reload
-- :t // gets type

mypi :: Double
mypi = 3.141592

-- bird-style programming:
-- comments .. 
-- > <code>
-- > <more-code>
-- more comments .. 

-- div
-- mod
-- for negative numbers: mod (-5) 3
-- sqrt
-- exp

-- no functions with random-length arguments

f x y = sqrt(x * x + y * y)

-- sqrt 4 * 2 == (sqrt 4) * 2

-- function signature
f :: Double -> Double -> Double
f x y = sqrt(x * x + y * y)

-- fundamental types: Int, Integer, Bool (True, False), Char ('s', 'c'), Float, Double
-- Int and Integer -> Integral
-- Float and Double -> Floating
-- Integral and Floating -> Num

-- if-else

(if 2<=3 then "iei" else "nay")

saySign :: (Num a, Ord a) => a -> String
saySign x = if x < 0
			then "Negative"
			else "Positive"

saySign2 :: (Num a, Ord a) => a -> String
saySign2 x
	| x < 0 = "Negative"
	| x == 0 = "Zero"
	| otherwise = "Positive"

