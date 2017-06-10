-- Задача 1. (10 т.) Да се напише функция findColumns, която по дадена матрица от числа намира броя на колоните, 
-- за които е вярно, че всичките им елементи се срещат в някой от редовете на матрицата.
-- Пример: findColumns [[1,4,3],[4,5,6],[7,4,9]] → 1
-- findColumns :: [[Int]] -> Int
-- findColumns m = 

-- transpose' :: [[Int]] -> [[Int]]
-- transpose' [] = []
-- transpose' m = (map head m) : transpose' (map (1 `drop`) m)

combine f g h = \x -> h (f x) (g x)