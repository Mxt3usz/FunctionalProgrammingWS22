import Test.QuickCheck
-- min factor k>=2 of n>=2  n = k * m

factor n = f n 2
        where
            f x y | x `mod` y == 0 = y
                  | otherwise = f x (y+1)


factor' n = f n 2 1
        where
            f x y z | y * z == x = y 
                    | y * z > x = f x (y+1) 1
                    | otherwise = f x y (z+1)



prop_fac n = (n >= 2) ==> factor n == factor' n
