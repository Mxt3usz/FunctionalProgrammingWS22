

-- (>>=) [a] -> (a -> [b]) -> [b]
-- return a -> [a]

import SimplePrelude as S
import Prelude (undefined)

pairs :: [a] -> [b] -> [(a,b)]

pairs xs ys = xs >>= \x -> ys >>= \y -> return (x,y)


-- list monad for knight movements


knight r c = do
        (r',c') <- [(r-2,c+1),(r-1,c+2),(r+1,c+2),(r+2,c+1),
                    (r+2,c-1),(r+1,c-2),(r-1,c+2),(r-2,c-1)]
        
