
undup [] = []
undup [x] = [x]
undup (x:xs) | not(filter' x xs) = x : undup xs
             | otherwise = undup xs
            where
            filter' a [] = False
            filter' a (s:as) | a /= s = filter' a as  
                             | a == s = True