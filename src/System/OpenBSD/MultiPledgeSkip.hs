{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE QualifiedDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StandaloneKindSignatures   #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module System.OpenBSD.MultiPledgeSkip ( trivial
                                      , runPledge
                                      , Pledge (..)
                                      , (System.OpenBSD.MultiPledge.>>=)
                                      , (System.OpenBSD.MultiPledge.>>)
                                      ) where

import           Data.Set.Singletons
import           System.OpenBSD.Pledge.Internal as I (Promise (..), pledge)

import           Control.Monad.Base             (MonadBase)
import           Control.Monad.IO.Class         (MonadIO)
import           Control.Monad.Trans.Class      (MonadTrans, lift)
import qualified Data.Set                       as S
import           Data.Singletons                (SingI, fromSing, sing)

-- | The wrapper for 'IO' actions
newtype Pledge
     (zs :: [Promise]) -- ^ The promises that need to be avaiable
                       -- after this action; best left polymorphic
     (ps :: [Promise]) -- ^ The promises required by this action
     m -- ^ The underlying monad. Combinators will require
                 -- this to be a 'MonadIO' instance
     a
  = Pledge { getAction :: m a }
  deriving (Functor, Foldable, Traversable)

deriving newtype instance (Monad m) => Monad (Pledge zs ps m)
deriving newtype instance (Applicative m) => Applicative (Pledge zs ps m)
deriving newtype instance (MonadIO m) => MonadIO (Pledge zs ps m)
deriving newtype instance (MonadBase b m) => MonadBase b (Pledge zs ps m)

instance MonadTrans (Pledge zs ps) where
  lift = Pledge

-- | Wrap an action that requires no promises
trivial :: m a -> Pledge zs '[] m a
trivial = Pledge

-- | Monad-like bind. The required promises of the composite action
-- are simply the join of the required promises of the
-- constituents. Note how the set of promises required after the
-- nullary action includes those required by the unary one.
--
-- Before executing the unary action, this executes a 'pledge' call to
-- shrink the promises to 'zs `Union` ps', i.e., those required by it
-- and any later actions.

class HasBind zs ps qs where
  (>>=) :: Pledge (zs `Union` ps) qs m a
        -> (a -> Pledge zs ps m b)
        -> Pledge zs (ps `Union` qs) m b

instance {- OVERLAPPABLE -} HasBind zs ps ps where
  (>>=) :: (Monad m)
        => Pledge (zs `Union` ps) ps m a
        -> (a -> Pledge zs (ps `Union` ps) m b)
        -> Pledge zs ps m b
  (>>=) a f = getAction a Prelude.>>= f

instance HasBind zs ps qs where
  (>>=) :: forall m a b.
           ( MonadIO m, SingI zs
           , SingI ps, SingI qs
           )
        => Pledge (zs `Union` ps) qs m a
        -> (a -> Pledge zs ps m b)
        -> Pledge zs (ps `Union` qs) m b
  (>>=) a f = Pledge $ do
    a' <- getAction a
    pledge $ S.fromList $ fromSing $ sing @zs `sUnion` sing @ps
    getAction $ f a'

(>>) :: forall zs ps m qs a b.
        ( MonadIO m, SingI zs, SingI ps, SingI qs
        )
     => Pledge (zs `Union` ps) qs m a
     -> Pledge zs ps m b
     -> Pledge zs (ps `Union` qs) m b
(>>) a a' = a System.OpenBSD.MultiPledgeSkip.>>= (const a')

-- | Run the pledged action after shrinking the promise set to
-- 'ps'. Note how we are forcing the 'zs' parameter to '[]'. After
-- this all promises are removed, so this function should be the last
-- call in main, afterward only pure computation can happen.
runPledge :: forall m ps a.
             (SingI ps, MonadIO m)
          => Pledge '[] ps m a
          -> m a
runPledge (Pledge a) = do
  _ <- pledge $ S.fromList $ fromSing $ sing @ps
  v <- a
  _ <- pledge $ S.fromList []
  pure v
