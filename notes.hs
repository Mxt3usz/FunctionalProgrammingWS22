import System.Environment (getArgs)


-- function composition
-- outer functions gets inner functions return value as argument
h = f . g
    where 
        f x = x + 1
        g x = x * 2


-- bind operator
-- allows for sequential evaluating 

h' = f' >>= \ns -> return (g' ns)
    where
        f' e = e + 1
        g' e = e * 2
    
-- other bind
-- basically ingnores ma

ma >>- mb = ma >>= \ _ -> mb


-- do notation

h'' l = do
    let f'' c = c + 1
    let g'' b = b * 2
    return (g'' (f'' l))


-- if u make string as input does weird stuff

seq' ios = foldr (\iox ioxs -> iox >>= \x -> ioxs >>= \xs -> return(x:xs)) (return []) ios


seq'' ios = foldr (\iox ioxs -> iox:ioxs) [] ios


seq''' ios = seq' ios >> return ()

--seq''''  = (>> return ()) . seq' 



d = (\[x] -> [x] >>= (return [x])) 



listOfFuns = map (*) [0 ..]



-- $ function aplication
-- sum (filter (> 10) (map (*2) [2..10]))

-- -> sum $ filter (> 10) $ map (*2) [2..10]

-- map ($ 3) [(4+), (10*), (^2), sqrt]  
-- -> [7.0,30.0,9.0,1.7320508075688772] 


-- function compostition

-- map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]  

-- map (negate . abs) [5,-3,-6,7,-3,2,-19,24]  way more concise

-- ghci> map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]  

-- ghci> map (negate . sum . tail) [[1..5],[3..6],[1..7]]  