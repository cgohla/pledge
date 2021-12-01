{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module System.OpenBSD.Pledge(runPledge, module I, Pledge(..), pledgePutStrLn) where

import Data.Text (Text)
import qualified Data.Text.IO as T (putStrLn)
import           Control.Effect                 (Effect, Inv, Plus, Unit,
                                                 return, (>>=))
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Control.Monad.Writer.Class     (tell)
import qualified Data.Set                       as S (fromList)
import           Language.Haskell.TH            (Body (..), Con (..), Dec (..),
                                                 Exp (..), Info (..), Pat (..),
                                                 Q, Type (..), mkName, nameBase,
                                                 reify)
import           System.OpenBSD.Pledge.Internal as I (Promise (..), pledge)

-- Here goes the fun stuff, i.e., the type level API

-- TODO this is probably an 'indexed monad'. perhaps use graded monads
-- defined in monad-effect. ADD actually, there should only be a
-- graded monad instance if the wrapped value is in a monad.

-- TODO write example programs
newtype Pledge m (ps :: [Promise]) a = Pledge { getAction :: m a } 

instance Functor m => Functor (Pledge m ps) where
  fmap f (Pledge a) = Pledge $ fmap f a

instance (Applicative m) => Applicative (Pledge m ps) where
  pure = Pledge . pure
  (<*>) (Pledge f) (Pledge a) = Pledge $ f <*> a

type family Concat (ps :: [a]) (ps' :: [a]) :: [a]
type instance Concat '[] ps = ps
type instance Concat (p ': ps) ps' = (p ': Concat ps ps')

instance (Monad m) => Effect (Pledge m) where
  type instance Unit (Pledge m) = '[]
  type instance Plus (Pledge m) ps ps' = Concat ps ps'
  type instance Inv (Pledge m) _ _ = Monad m
  return = Pledge . Prelude.return
  (>>=) (Pledge a) f = Pledge $ a Prelude.>>= (getAction . f)

-- | Extract a value level list of promises, whithouf running the action.
getPromises :: (CollectPromise ps) => Pledge m ps a -> [Promise]
getPromises = fst . collectPromise

-- | Apply all promises using pledge(2) and execute the action. Note
-- that there is no way to revert promises, hence anything run after
-- this is subject to the same restrictions. So this should be the
-- last call in main.
runPledge :: (MonadIO m, CollectPromise ps) => Pledge m ps a -> m a
runPledge p = do
  let (ps, a) = collectPromise p
  pledge $ S.fromList ps
  a

-- | Example action
pledgePutStrLn :: (MonadIO m) => Text -> Pledge m '[ 'Stdio ] ()
pledgePutStrLn = Pledge . liftIO . T.putStrLn

-- | Broken action. This should crash the program because no promise
-- is declared, even though 'Stdio' is needed to print to std out.
pledgePutStrLn' :: (MonadIO m) => Text -> Pledge m '[ ] ()
pledgePutStrLn' = Pledge . liftIO . T.putStrLn

class CollectPromise ps where
  collectPromise :: Pledge m ps a -> ([Promise], m a)

instance CollectPromise '[] where
  collectPromise (Pledge a) = ([], a)

popPromise :: SingPromise p => Pledge m (p ': ps) a -> (SPromise p, Pledge m ps a)
popPromise (Pledge a) = (sing, Pledge a)

instance (ConcretePromise p, SingPromise p, CollectPromise ps) => CollectPromise (p ': ps) where
  collectPromise p = do
    let (sp, p') = popPromise p
    tell $ pure $ concrete sp
    collectPromise p'

data family SPromise (p :: Promise) :: *

class ConcretePromise (p :: Promise) where
  concrete :: SPromise p -> Promise

class SingPromise (p :: Promise) where
  sing :: SPromise p

$(let
     promiseConstructors :: Q [Con]
     promiseConstructors = do
       p <- reify ''Promise
       case p of
         TyConI dec -> pure $ case dec of
           DataD _ _ _ _ cs _-> cs
           _                  -> fail "only the DataD case is handled"
         _ -> fail "only the TyConI case is handled"
     mkSPromiseInst :: Con -> Q Dec
     mkSPromiseInst (NormalC cName _) =
       let
         cxt = []
         binders = Nothing
         typ = (AppT (ConT ''SPromise) (PromotedT cName))
         kind = Nothing
         cons = [NormalC (mkName $ "S" <> nameBase cName) []]
         derivings = [] -- TODO add standard TC derivations
       in
         pure $ DataInstD cxt binders typ kind cons derivings
     mkSPromiseInst _ = fail "only the NormalC case is handled"
     mkConcreteInst :: Con -> Q Dec
     mkConcreteInst (NormalC cName _) =
       let
         overlap = Nothing
         cxt = []
         typ = AppT (ConT ''ConcretePromise) (PromotedT cName)
         dec = [ValD (VarP 'concrete) (NormalB (LamE [WildP] (ConE cName))) [] ]
       in
         pure $ InstanceD overlap cxt typ dec
     mkConcreteInst _ = fail "only the NormalC case is handled"
     mkSingInst :: Con -> Q Dec
     mkSingInst (NormalC cName _) =
       let
         overlap = Nothing
         cxt = []
         typ = AppT (ConT ''SingPromise) (PromotedT cName)
         dec = [ValD (VarP 'sing) (NormalB $ ConE $ mkName $ "S" <> nameBase cName) [] ]
       in
         pure $ InstanceD overlap cxt typ dec
     mkSingInst _ = fail "only the NormalC case is handled"
     sPromiseInsts :: Q [Dec]
     sPromiseInsts = traverse mkSPromiseInst =<< promiseConstructors
     concreteInsts :: Q [Dec]
     concreteInsts = traverse mkConcreteInst =<< promiseConstructors
     singInsts :: Q [Dec]
     singInsts = traverse mkSingInst =<< promiseConstructors
  in do
     ps <- sPromiseInsts
     cs <- concreteInsts
     ss <- singInsts
     pure $ ps <> cs <> ss
 )

