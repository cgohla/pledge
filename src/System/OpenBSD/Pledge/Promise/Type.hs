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

module System.OpenBSD.Pledge.Promise.Type (Promise(..), promiseToString) where

import qualified Data.Char          as C (toLower)
import           Data.Singletons.TH (genSingletons)

data Promise =
  Stdio | Rpath | Wpath | Cpath | Dpath | Tmppath | Inet | Mcast |
  Fattr | Chown | Flock | Unix | Dns | Getpw | Sendfd | Recvfd | Tape
  | Tty | Proc | Exec | Prot_exec| Settime | Ps | Vminfo | Id | Route
  | Wroute | Audio | Video | Bpf| Unveil | Error
  deriving (Show, Eq, Enum, Ord)

genSingletons [ ''Promise ]

promiseToString :: Promise -> String
promiseToString = fmap C.toLower . show
