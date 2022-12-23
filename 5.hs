isNothing' :: Maybe a -> Bool

isNothing' Nothing = True
isNothing' (Just a) = False

maybe' :: b -> (a -> b) -> Maybe a -> b

maybe' b f (Just a) = f a
maybe' b f Nothing = b


isJust' :: Maybe a -> Bool

isJust' (Just a) = True
isJust' Nothing = False

unfoldr' :: (b -> Maybe (a, b)) -> b -> [a]


unfoldr' f seed  = case f seed of -- pattern match on result
                    Just (a,b) -> a : unfoldr' f b
                    Nothing -> []

map' f xs = unfoldr' f' xs
            where 
                f' [] = Nothing
                f' (x:xs) = Just (f x,xs)

range' m n = unfoldr' f' m 
    where 
        f' x | x == n+1 = Nothing
             | otherwise = Just(x,x+1)


