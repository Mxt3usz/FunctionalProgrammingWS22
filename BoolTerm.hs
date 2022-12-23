-- Copyright 2022 University of Freiburg
-- Janek Spaderna <janek.spaderna@pluto.uni-freiburg.de>
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module BoolTerm where

import Control.Exception (evaluate)
import Control.Monad (void)
import GHC.Generics (Generic)
import Test.QuickCheck

-------------------------------------------------------------------------------
-- BoolTerm definition

data BoolTerm
  = T
  | F
  | Var Char
  | Not BoolTerm
  | Conj BoolTerm BoolTerm
  | Disj BoolTerm BoolTerm
  deriving (Eq, Show, Generic)

type Position = [Integer]




-- -> Pos(b) = [[1]]
-------------------------------------------------------------------------------
-- Implement the functions here.
pos :: BoolTerm -> [Position]
pos term = [] : f''(f' term [])
  where
    f'' [] = []
    f'' (x:xs) | null x = f'' xs
               | otherwise = x : f'' xs
    f' F l = [[]]
    f' T l = [[]]
    f' (Var _) l = [[]]
    f' (Not a) l = [l ++ [1]]
    f' (Disj a b) l =  (l ++ [1]) : f' a (l ++ [1]) ++ (l ++ [2]) : f' b (l ++ [2])
    f' (Conj a b) l =  (l ++ [1]) : f' a (l ++ [1]) ++ (l ++ [2]) : f' b (l ++ [2])

(|.) :: BoolTerm -> Position -> BoolTerm
(|.) term p   | null p = term
              | otherwise = f' term 0
    where
      f' (Not a) l = a

      f' (Disj a b) l | p !! l == 1 && l+1 == length p = a
                      | p !! l == 2 && l+1 == length p = b
                      | p !! l == 1 = f' a (l+1)
                      | p !! l == 2 = f' b (l+1)
      
      f' (Conj a b) l | p !! l == 1 && l+1 == length p = a
                      | p !! l == 2 && l+1 == length p = b
                      | p !! l == 1 = f' a (l+1)
                      | p !! l == 2 = f' b (l+1)


b = Conj (Disj (Var 'c') (Var 'd')) T

a = Conj (Disj (Var 'c') (Not(Var 'd'))) T

c = Not T
-- -> ∧ (v(c,d),True)
replace :: BoolTerm -> BoolTerm -> Position -> BoolTerm
replace term term' [] = term' 
replace (Not a) term' p = Not term'
replace term term' p = f' term 0
    where
      -- first 2 remove Not pattern matchting
      f' (Not a) l = Not term'

      f' (Disj a b) l | p !! l == 1 && l+1 == length p = Disj term' b
                      | p !! l == 2 && l+1 == length p = Disj a term'
                      | p !! l == 1 = Disj (f' a (l+1)) b
                      | p !! l == 2 = Disj a (f' b (l+1))
      
      f' (Conj a b) l | p !! l == 1 && l+1 == length p = Conj term' b
                      | p !! l == 2 && l+1 == length p = Conj a term'
                      | p !! l == 1 = Conj (f' a (l+1)) b
                      | p !! l == 2 = Conj a (f' b (l+1)) 

-------------------------------------------------------------------------------
-- Properties

-- | All terms should contain Îµ in their set of positions.
prop_pos_empty :: BoolTerm -> Bool
prop_pos_empty t = [] `elem` pos t

-- | Checks that all positions returned from `pos` resolve to valid subterms.
prop_pos_valid :: BoolTerm -> Property
prop_pos_valid t = forAll (elements (pos t)) $ \p ->
  ioProperty $ void $ evaluate (t |. p)

-- | 1. t_pq = (t|p)|q
prop_at_split :: BoolTerm -> Property
prop_at_split t =
  forAll (elements (pos t)) $ \pq ->
    forAll (choose (0, length pq)) $ \n ->
      let (p, q) = splitAt n pq
       in t |. p |. q == t |. pq

-- | 2. (s[t]p)|pq = t|q
prop_replace_split :: BoolTerm -> BoolTerm -> Property
prop_replace_split s t =
  forAll (elements (pos s)) $ \p ->
    forAll (elements (pos t)) $ \q ->
      replace s t p |. (p ++ q) == t |. q

-- | 3. (s[t]p)[r]pq = s[t[r]q]p
prop_replace_twice :: BoolTerm -> BoolTerm -> BoolTerm -> Property
prop_replace_twice s t r =
  forAll (elements (pos s)) $ \p ->
    forAll (elements (pos t)) $ \q ->
      replace (replace s t p) r (p ++ q) == replace s (replace t r q) p

-------------------------------------------------------------------------------
-- Allow quantification over `BoolTerm` values.

instance Arbitrary BoolTerm where
  arbitrary = sized go
    where
      go :: Int -> Gen BoolTerm
      go 0 = oneof [pure T, pure F, Var <$> choose ('a', 'z')]
      go n =
        oneof
          [ Not <$> go (n - 1),
            binary (n - 1) Conj,
            binary (n - 1) Disj
          ]

      binary :: Int -> (BoolTerm -> BoolTerm -> BoolTerm) -> Gen BoolTerm
      binary n f = do
        !a <- invE 0.25 <$> choose (0, 1)
        !b <- invE 0.25 <$> choose (0, 1)
        let rescaled x = go $ round $ x * fromIntegral n / (a + b)
        f <$> rescaled a <*> rescaled b

      invE :: Double -> Double -> Double
      invE lambda u = (-log (1 - u)) / lambda

  shrink = genericShrink