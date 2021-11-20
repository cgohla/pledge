{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module System.OpenBSD.Pledge where

import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Control.Monad.Writer           (WriterT, runWriterT)
import           Control.Monad.Writer.Class     (MonadWriter, tell)
import qualified Data.Set                       as S (fromList)
import           Language.Haskell.TH            (Body (..), Con (..), Dec (..),
                                                 Exp (..), Info (..), Pat (..),
                                                 Q, Type (..), mkName, nameBase,
                                                 reify)
import           System.OpenBSD.Pledge.Internal (Promise (..), pledge)

-- Here goes the fun stuff, i.e., the type level API

-- TODO this is probably an 'indexed monad'. perhaps use graded monads defined in monad-effect
-- TODO write example programs
data PledgeIOT (ps :: [Promise]) m a = PledgeIOT (m a)

-- | Extract a value level list of promises, whithouf running the action.
getPromises :: (CollectPromise ps, Monad m) => PledgeIOT ps (WriterT [Promise] m) a -> m [Promise] -- TODO accidentally seems to run the action anyway
getPromises = fmap snd . runWriterT . collectPromise

-- | Apply all promises using pledge(2) and execute the action. Note
-- that there is no way to revert promises, hence anything run after
-- this is subject to the same restrictions. So this should be the
-- last call in main.
runPledge :: (MonadIO m, CollectPromise ps) => PledgeIOT ps (WriterT [Promise] m) a -> m a
runPledge p = do
  (a, w) <- runWriterT $ collectPromise p
  pledge $ S.fromList w
  pure a

-- | Example action
pledgePutStrLn :: (MonadIO m) => String -> PledgeIOT '[ 'Stdio ] m ()
pledgePutStrLn = PledgeIOT . liftIO . putStrLn

-- | Broken action. This should crash the program
pledgePutStrLn' :: (MonadIO m) => String -> PledgeIOT '[ ] m () -- TODO but it doesn't. Same problem as 'getPromises'
pledgePutStrLn' = PledgeIOT . liftIO . putStrLn

class CollectPromise ps where
  collectPromise :: (MonadWriter [Promise] m)
                 => PledgeIOT ps m a
                 -> m a

instance CollectPromise '[] where
  collectPromise (PledgeIOT b) = b

instance (ConcretePromise p, SingPromise p, CollectPromise ps) => CollectPromise (p ': ps) where
  collectPromise p = do
    let (sp, ps) = popPromise p
    tell $ pure $ concrete sp
    collectPromise ps
      where
        popPromise :: SingPromise p => PledgeIOT (p ': ps) m a -> (SPromise p, PledgeIOT ps m a)
        popPromise (PledgeIOT a) = (sing, PledgeIOT a)

data family SPromise (p :: Promise) :: *

class ConcretePromise (p :: Promise) where
  concrete :: SPromise p -> Promise

-- sing needs to be a type class method giving the unique constructor of SPromise p for each p

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

