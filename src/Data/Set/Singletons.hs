{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE TypeOperators   #-}
module Data.Set.Singletons where

import           Data.List.Singletons hiding (Union)
import           Data.Ord.Singletons

type IsSet a = (a ~ AsSet a)

type AsSet a = Nub (Sort a)

asSet :: (SOrd k) => SList (a :: [k]) -> SList (AsSet a)
asSet as = sNub $ sSort as

type Union a b = AsSet (a ++ b)

union :: (SOrd k) => SList (a :: [k]) -> SList (b :: [k]) -> SList (Union a b)
union a b = asSet $ a %++ b
