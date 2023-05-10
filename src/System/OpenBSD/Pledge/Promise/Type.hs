{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveFunctor            #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE InstanceSigs             #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}
module System.OpenBSD.Pledge.Promise.Type (Promise(..), promiseToString, SPromise (..)) where

import           Data.Bool.Singletons
import qualified Data.Char            as C (toLower)
import           Data.Eq.Singletons
import           Data.List.Singletons
import           Data.Ord.Singletons
import           Data.Singletons.TH   (genSingletons, singEqInstance,
                                       singOrdInstance)

data Promise =
  Stdio | Rpath | Wpath | Cpath | Dpath | Tmppath | Inet | Mcast |
  Fattr | Chown | Flock | Unix | Dns | Getpw | Sendfd | Recvfd | Tape
  | Tty | Proc | Exec | Prot_exec| Settime | Ps | Vminfo | Id | Route
  | Wroute | Audio | Video | Bpf| Unveil | Error
  deriving (Show, Eq, Enum, Ord)

genSingletons [ ''Promise ]
singEqInstance ''Promise
singOrdInstance ''Promise
-- singShowInstance ''Promise

promiseToString :: Promise -> String
promiseToString = fmap C.toLower . show

