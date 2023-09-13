{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
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

module System.OpenBSD.MultiPledge ( trivial
                                  , runPledge
                                  , Pledge (..)
                                  , (System.OpenBSD.MultiPledge.>>=)
                                  , (System.OpenBSD.MultiPledge.>>)
                                  , unpledge
                                  , runInPledge
                                  ) where

import           Data.Set.Singletons
import           System.OpenBSD.Pledge.Internal as I (Promise (..), pledge)

import           Control.Monad.Base             (MonadBase)
import           Control.Monad.IO.Class         ()
import           Control.Monad.IO.Unlift
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

instance MonadUnliftIO (Pledge zs ps IO) where
  withRunInIO f = Pledge $ f getAction

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
(>>=) :: forall zs ps m qs a b.
        ( MonadIO m, SingI zs, SingI ps, SingI qs
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
(>>) a a' = a System.OpenBSD.MultiPledge.>>= const a'

-- | Run the pledged action after shrinking the promise set to
-- 'ps'. Note how we are forcing the 'zs' parameter to '[]'. After
-- this all promises are removed, so this function should be the last
-- call in main, afterward only pure computation can happen.
runPledge :: forall m ps a.
             (SingI ps, MonadIO m)
          => Pledge '[ 'Stdio] ps m a
          -> m a
runPledge a = do
  _ <- pledge $ S.fromList $ fromSing $ sing @ps
  v <- getAction a
  _ <- pledge $ S.fromList $ fromSing $ sing @'[ 'Stdio]
  pure v

-- | Run a function underneath the Pledge wrapper. Care must be taken
-- not to actually excute privileged action here.
runInPledge :: (m a -> m b) -> Pledge zs ps m a -> Pledge zs ps m b
runInPledge f = Pledge . f . getAction

unpledge :: forall zs ps (m :: * -> *) a.
            ( MonadIO m
            , Applicative m
            , SingI zs
            , SingI ps
            , ps ~ ps `Union` ps
            )
         => Pledge (zs `Union` ps) ps m a -> Pledge zs ps m a
unpledge a = a System.OpenBSD.MultiPledge.>>= (pure :: a -> Pledge zs ps m a)
