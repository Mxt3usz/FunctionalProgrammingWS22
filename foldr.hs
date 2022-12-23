
or' :: [Bool] -> Bool

or' xs = foldr' (||) False xs

and' :: [Bool] -> Bool

and' xs = foldr' (&&) True xs

h xs = foldr' (:) [] xs
g xs ys = foldr' (:) ys xs



filter' f xs = foldr' (\ x y -> case () of 
                                _| f x -> x : y
                                 | otherwise -> y) [] xs

redumps xs = foldr' (\ x y -> case () of
                                _| null(y) -> x : y
                                 | x /= y !! 0 -> x : y 
                                 | otherwise -> y) [] xs

avg :: [Double] -> Double
avg xs = foldr' (\ x y -> x + y) 0 xs / (fromIntegral(length xs))

foldr' f z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)

foldl' :: (b -> a -> b) -> b -> [a] -> b

foldl' f z [] = z
foldl' f z xs = f (foldl' f z (init xs)) (last xs)


