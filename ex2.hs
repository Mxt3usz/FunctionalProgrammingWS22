import Text.Read

-- stack-based calculator
stack :: [Integer]
stack = [8,6,3,2,0] -- stack filled with indefinte amount of 0's

pop [] = [0] -- pattern matching
pop [x] = [0]
pop (x:xs) = xs
push x n = n:x


add (y:(x:xs)) = x+y:y:x:xs

substract (y:(x:xs)) = y-x:y:x:xs

multiply (y:(x:xs)) = x*y:y:x:xs

neg (x:xs) = -x:xs

dup (x:xs) = x:x:xs


readCommand :: String -> [Integer] -> [Integer]
readCommand "add" ys = add ys
readCommand "substract" ys = substract ys
readCommand "multiply" ys = multiply ys
readCommand "neg" ys = neg ys
readCommand "dup" ys = dup ys
readCommand "pop" ys = pop ys
readCommand ('p':'u':'s':'h':' ':a) ys = push ys (read a)


