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
                           ) where

import           Data.List.Singletons (Nub, SList, Sort, Union, sNub, sSort,
                                       sUnion, (%++))
import           Data.Ord.Singletons  (SOrd)

type IsSet a = (a ~ AsSet a)

type AsSet a = Nub (Sort a)

asSet :: (SOrd k) => SList (a :: [k]) -> SList (AsSet a)
asSet as = sNub $ sSort as
