main :: IO ()

main = do
    putStrLn "Choose a number between 1 and 100!"
    let f x f' off n att = do
            putStrLn ("Is it " ++ show x ++ " ?")
            y <- getLine
            case y of
                "yes" -> putStrLn ("I won in " ++ show att ++ " attempts")
                "smaller" -> f (x - f' 0 off n) f' (f' 0 off n) 0 (att + 1)
                "greater" -> f (x + f' 1 off n) f' (f' 1 off n) 1 (att + 1)     
    f 50 (\ z z' z'' -> case () of
                            _ | z /= z'' && z'' /= -1 -> z' - 1 
                              | z' == 0 -> 1
                              |otherwise -> z')5 (-1) 0
                            
        