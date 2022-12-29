import Test.QuickCheck

guard :: Bool -> [()]
guard b | b = return ()
        | not b = []

prop_guardTrue, prop_guardFalse :: [Integer] -> [Integer] -> Bool
prop_guardTrue xs ys = (xs >> guard True >> ys) == (xs >> ys)
prop_guardFalse xs ys = (xs >> guard False >> ys) == []
