import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode (ExitFailure, ExitSuccess))

contains [] _ = False
contains (s1:xs) (s2:ys) | s1 == s2 && align xs ys = True
                         | otherwise = contains xs (s2:ys)
                where 
                    align _ [] = True
                    align [] _ = False
                    align (a:as) (b:bs) | a == b = align as bs
                                        | otherwise = False 

grepString :: Bool -> String -> String -> String
grepString b s1 s2 = f (lines s2) s1 b
                    where 
                        f [] b c = ""
                        f (a:as) b c | c && contains a b  = a ++ f as b c
                                     | not c && not (contains a b) = a ++ f as b c
                                     | otherwise = f as b c

main = do
    cmd <- getArgs
    let look (x:xs) pattern c | contains x pattern = c
                        | null xs = -1
                        | otherwise = look xs pattern (c+1)
        v_flag = look cmd "-v" 0
        file_flag = look cmd ".txt" 0
   
    let format x  | length x > 3 || length x < 2 = -1
                    | length x == 3 && v_flag /= 0 = -1
                | otherwise = 1
    case format cmd of 
        -1 -> exitWith (ExitFailure 42)
        1 -> do
            case file_flag of
                -1 -> putStrLn (grepString (v_flag == -1) (cmd !! 0 ++ "\n") (cmd !! 1))
                _ -> do
                    file <- readFile (cmd !! file_flag)
                    putStrLn (grepString (v_flag == -1) (cmd  !! (file_flag-1)) file)
                        
            