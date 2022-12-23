

data Term = Val Integer | Bin Term Op Term
        deriving(Show,Eq)

data Op = Add | Mult | Sub | Div
        deriving(Show,Eq)


calc x Add y = x + y
calc x Sub y = x - y
calc x Mult y = x * y
calc x Div y = x `div` y        

eval :: Term -> Maybe Integer
eval (Val a) = Just a
eval (Bin x op y) = case eval x of
                        Nothing -> Nothing
                        Just x -> case eval y of
                                Nothing -> Nothing
                                Just y -> if y == 0 && op == Div
                                        then Nothing
                                        else
                                        Just (calc x op y)

eval_monoad (Val a) = return a
eval_monoad (Bin x op y) = eval_monoad x >>= \n -> eval_monoad y >>= \m -> return (calc n op m)

-- do noation even shorter

eval_do (Val a) = return a
eval_do (Bin x op y) = do
                        z <- eval_do x
                        u <- Nothing
                        return (z)

