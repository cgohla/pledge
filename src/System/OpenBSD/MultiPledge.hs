{-# LANGUAGE QualifiedDo              #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveFunctor            #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}

module System.OpenBSD.MultiPledge ( trivial
                                  , bind
                                  , runPledge
                                  , Pledge (..)
                                  , (System.OpenBSD.MultiPledge.>>=)
                                  , (System.OpenBSD.MultiPledge.>>)
                                  ) where

import           System.OpenBSD.Pledge.Internal as I (Promise (..), pledge)

import           Control.Monad.IO.Class         (MonadIO)
import           Data.List.Singletons
import qualified Data.Set                       as S
import           Data.Singletons                (SingI, fromSing, sing)

-- | The wrapper for 'IO' actions
newtype Pledge m -- ^ The underlying monad. Combinators will require
                 -- this to be a 'MonadIO' instance
     (zs :: [Promise]) -- ^ The promises that need to be avaiable
                       -- after this action; best left polymorphic
     (ps :: [Promise]) -- ^ The promises required by this action
     a
  = Pledge { getAction :: m a }
  deriving (Functor)

instance (Functor m, Applicative m) => Applicative (Pledge m zs ps) where
  pure = Pledge . pure -- ^ Idealy we should avoid using this, because
                       -- it inflates the promise set
  (<*>) (Pledge f) (Pledge a) = Pledge $ f <*> a

-- | Wrap an action that requires no promises
trivial :: m a -> Pledge m zs '[] a
trivial = Pledge

-- | Monad-like bind. The required promises of the composite action
-- are simply the join of the required promises of the
-- constituents. Note how the set of promises required after the
-- nullary action includes those required by the unary one.
--
-- Before executing the unary action, this executes a 'pledge' call to
-- shrink the promises to 'zs ++ ps', i.e., those required by it and
-- any later actions.
bind :: forall m zs ps qs a b.
        (MonadIO m, SingI zs, SingI ps, SingI qs)
     => (a -> Pledge m zs ps b)
     -> Pledge m (zs ++ ps) {- concat order? -} qs a
     -> Pledge m zs (ps ++ qs) b
bind f a = Pledge $ do
  a' <- getAction a
  pledge $ S.fromList $ fromSing $ sing @zs %++ sing @ps
  getAction $ f a'

(>>=) :: forall m zs ps qs a b.
        (MonadIO m, SingI zs, SingI ps, SingI qs)
     => Pledge m (zs ++ ps) {- concat order? -} qs a
     -> (a -> Pledge m zs ps b)
     -> Pledge m zs (ps ++ qs) b
(>>=) = flip bind

(>>) :: forall m zs ps qs a b.
        (MonadIO m, SingI zs, SingI ps, SingI qs)
     => Pledge m (zs ++ ps) {- concat order? -} qs a
     -> Pledge m zs ps b
     -> Pledge m zs (ps ++ qs) b
(>>) a a' = a System.OpenBSD.MultiPledge.>>= (const a')

-- | Run the pledged action after shrinking the promise set to
-- 'ps'. Note how we are forzing the 'zs' parameter to '[]'.
runPledge :: forall m ps a.
             (SingI ps, MonadIO m)
          => Pledge m '[] ps a
          -> m a
runPledge (Pledge a) = do
  _ <- pledge $ S.fromList $ fromSing $ sing @ps
  a
