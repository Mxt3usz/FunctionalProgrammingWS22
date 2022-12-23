-- 1.
maxi :: Integer -> Integer -> Integer
maxi x y = if x > y then x else y

mini :: Integer -> Integer -> Integer
mini x y = if x < y then x else y

-- 2.
max3 x y z | x >= y && x >= z = x
           | y >= x && y >= z = y
           | otherwise = z

max3Tupled (x,y,z) | x >= y && x >= z = x
           | y >= x && y >= z = y
           | otherwise = z

-- 3.

med x y z | x >= y && x <= z  || x <= y && x >= z = x
    | y >= x && y <= z  || y <= x && y >= z = y
    | otherwise = z

propMax x y = maxi x y == max x y