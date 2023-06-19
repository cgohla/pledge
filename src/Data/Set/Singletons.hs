{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE TypeOperators   #-}
module Data.Set.Singletons ( IsSet
                           , AsSet
                           , asSet
                           , module Data.List.Singletons
                           , module Data.Ord.Singletons
                           , Union
                           ) where

import           Data.List.Singletons (Nub, SList, Sort, sNub, sSort,
                                       sUnion, (%++))
import qualified Data.List.Singletons as S (Union)
import           Data.Ord.Singletons  (SOrd)

type IsSet a = (a ~ AsSet a)

type AsSet a = Nub (Sort a)

-- | We need unions to be sorted, which the default implementation
-- does not do.
type Union a b = Sort (S.Union a b)

-- TODO need to take care of the singled functions as well.

asSet :: (SOrd k) => SList (a :: [k]) -> SList (AsSet a)
asSet as = sNub $ sSort as
