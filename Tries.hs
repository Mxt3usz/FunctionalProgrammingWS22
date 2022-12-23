-- Copyright 2022 University of Freiburg
-- Janek Spaderna <janek.spaderna@pluto.uni-freiburg.de>
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

module Trie where

import Data.Map qualified as Map
import Test.QuickCheck

data Trie = Trie Bool (Map.Map Char Trie)
  deriving (Eq, Show)

instance Semigroup Trie where
  (<>) = union

instance Monoid Trie where
  mempty = empty

-- | Returns an empty `Trie`.
empty :: Trie
empty = Trie False Map.empty

-- | Checks if the given `String` is a part of the provided `Trie`.
member :: String -> Trie -> Bool
member "" (Trie b _) = b
member (c : cs) (Trie _ ts) = maybe False (member cs) (Map.lookup c ts)

prop_EmptyHasNoMembers :: String -> Bool
prop_EmptyHasNoMembers s = not (s `member` empty)

-- | Merges two `Trie`s.
union :: Trie -> Trie -> Trie
union (Trie a ts) (Trie b us) = Trie (a || b) (Map.unionWith union ts us)

prop_UnionOfFromListIsFromList :: [String] -> [String] -> Bool
prop_UnionOfFromListIsFromList xs ys =
  fromList xs `union` fromList ys == fromList (xs ++ ys)

prop_UnionContainsAll :: [String] -> [String] -> Bool
prop_UnionContainsAll xs ys = all isMember xs && all isMember ys
  where
    trie = fromList xs `union` fromList ys
    isMember s = s `member` trie

-- | Adds the given prefix to every word inside the `Trie`.
--
-- > word `member` trie  ==>  (p ++ word) `member` prefix p trie
prefix :: String -> Trie -> Trie
prefix cs t
  -- Ensure that the resulting trie is minimal.
  | t == empty = empty
  | otherwise = foldr (\c -> Trie False . Map.singleton c) t cs

prop_PrefixContainsAll :: String -> [String] -> Bool
prop_PrefixContainsAll p xs = all isPrefixedMember xs
  where
    prefixed = prefix p (fromList xs)
    isPrefixedMember s = (p ++ s) `member` prefixed

prop_PrefixContainsPrefix :: String -> [String] -> String -> Bool
prop_PrefixContainsPrefix p xs w =
  w `member` fromList xs == (p ++ w) `member` prefix p (fromList xs)

prop_PrefixEmptyIsEmpty :: String -> Bool
prop_PrefixEmptyIsEmpty p = prefix p empty == empty

-- | Inserts the given `String` into a `Trie`.
insert :: String -> Trie -> Trie
insert s t = prefix s root `union` t
  where
    -- A `Trie` containing only the empty string.
    root = Trie True Map.empty

-- | Inserts the given `String` into a `Trie`.
--
-- Alternative, more direct, implementation.
insert2 :: String -> Trie -> Trie
insert2 "" (Trie _ ts) = Trie True ts
insert2 (c : cs) (Trie b ts) = Trie b $ case Map.lookup c ts of
  Just t -> Map.insert c (insert cs t) ts
  Nothing -> Map.insert c (insert cs empty) ts

prop_InsertedIsMember :: [String] -> String -> Bool
prop_InsertedIsMember xs s = s `member` insert s (fromList xs)

prop_InsertedIsMember_2 :: [String] -> String -> Bool
prop_InsertedIsMember_2 xs s = s `member` insert2 s (fromList xs)

-- | Removes the given `String` from the `Trie`.
--
-- NaÃ¯ve version of `delete` which does not result in a minimal `Trie`.
--
-- (Yes, you can use more than just the ASCII characters in indentifiers!)
deleteNaÃive :: String -> Trie -> Trie
deleteNaÃive "" (Trie _ ts) = Trie False ts
deleteNaÃive (c : cs) (Trie b ts) = Trie b ts'
  where
    -- Map.adjust :: (v -> v) -> k -> Map k v
    ts' = Map.adjust (deleteNaÃive cs) c ts

-- This property fails, because `deleteNaÃ¯ve` does not result in a minimal
-- trie.
--
-- prop_DeleteEverythingResultsInEmpty_naÃ¯ve :: [String] -> Bool
-- prop_DeleteEverythingResultsInEmpty_naÃ¯ve xs = foldr deleteNaÃ¯ve (fromList xs) xs == empty

prop_DeletedIsn'tMember_naive :: [String] -> String -> Bool
prop_DeletedIsn'tMember_naive xs s = not $ s `member` delete s (fromList xs)

-- | Removes the given `String` from the `Trie`.
--
-- This function ensures that the resulting tree is minimal, if the input tree
-- was minimal.
delete :: String -> Trie -> Trie
delete "" (Trie _ ts) = Trie False ts
delete (c : cs) (Trie b ts) = Trie b ts'
  where
    -- Map.update :: (v -> Maybe v) -> k -> Map k v
    --
    -- The key is dropped if the function returns `Nothing`.
    ts' = Map.update (reduce . delete cs) c ts
    reduce (Trie False us) | Map.null us = Nothing
    reduce t = Just t

prop_DeleteEverythingResultsInEmpty :: [String] -> Bool
prop_DeleteEverythingResultsInEmpty xs = foldr delete (fromList xs) xs == empty

prop_DeletedIsn'tMember :: [String] -> String -> Bool
prop_DeletedIsn'tMember xs s = not (s `member` delete s (fromList xs))

-- | Creates a `Trie` from a list of `String`s.
fromList :: [String] -> Trie
fromList = foldr insert empty

-------------------------------------------------------------------------------
-- Question 3
--
-- a) Give the new data type definition.

data Trie' a = Trie' Bool (Map.Map a (Trie' a))

-- b) Are there required typeclass constraints? How do the type signatures
--    change?
--
--    Trie    => Trie' a
--    String  => [a]
--
--    All functions, except `empty` and `prefix`, need a `Ord a` constraint.
--
-- c) How do the function implementations change?
--
--    No changes* necessary, as the differences are hidden inside the Map
--    functions through parametric polymorphism.
--
--    [*] the emptyness check in `prefix` either requires either an `Eq`
--        constraint on `a`, or a check Ã  la the version for `TrieMap`.

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

return []

checkAll :: IO Bool
checkAll = $quickCheckAll