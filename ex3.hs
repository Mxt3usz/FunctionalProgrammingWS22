
h (x:xs) = x -- extract first ele
t (x:xs) = xs -- remove first ele
l[x] = x
l (x:xs) = l xs  -- extract last ele

ini[x] = []
ini [x,y] = [x]
ini (x:xs)  | not(null(ini xs)) = x : ini xs
          | otherwise = ini xs  -- remove last element
-- min
mini [x] = x
mini (x:xs) | x < mini xs = x
            | otherwise = mini xs


le [] = 0
le (x:xs) = le xs + 1 -- length of list

andd [] = True
andd (x : xs) | not x  = False 
              | otherwise = andd xs  -- returns False if not all Bools are True 

rev [x] = [x]
rev (x:xs) | not(null(rev xs)) = rev xs ++ [x] -- append to the back of the list
           | otherwise = rev xs

plus :: [a] -> [b] -> [(a, b)]
plus [] [] = []
plus [x] [a] = [(x,a)]
plus [x] [a,b] = [(x,a)]
plus [x,y] [a] = [(x,a)]
plus (x:xs) (y:ys) | not(null(plus xs ys)) = (x,y):plus xs ys
                   | otherwise = plus xs ys

app [] [] = []
app [] ys = ys
app [x] ys = x:ys
app (x:xs) ys | not(null( app xs ys)) = x:app xs ys
                  | otherwise = app xs ys