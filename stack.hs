
stack = do
    let s lst = do
            print lst
            operation <- getLine
            case operation of
                "exit" -> putStr ""
                _ ->  s (f operation lst)
                    where
                        f ('p': 'u' : 's' : 'h' : ' ' : a) b  = read a : b  -- pattern match on operator
                        f _ [0] = [0] -- to do anything we have to have at least 2 elements
                        f "pop" (b:bs) = bs  
                        f "dup" b = b !! 0 : b
                        f "add" b = b !! 0 + b !! 1 : b
                        f "subtract" b = b !! 0 - b !! 1 : b
                        f "multiply" b = b !! 0 * b !! 1 : b
                        f "neg" b = b !! 0 * (-1) : b
                        f _ b = b
    s [0]

        
