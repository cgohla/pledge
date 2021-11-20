module System.OpenBSD.Pledge.Promise.Type (Promise(..), promiseToString) where

import qualified Data.Char as C (toLower)

data Promise =
  Stdio | Rpath | Wpath | Cpath | Dpath | Tmppath | Inet | Mcast |
  Fattr | Chown | Flock | Unix | Dns | Getpw | Sendfd | Recvfd | Tape
  | Tty | Proc | Exec | Prot_exec| Settime | Ps | Vminfo | Id | Route
  | Wroute | Audio | Video | Bpf| Unveil | Error
  deriving (Show, Eq, Enum, Ord)

promiseToString :: Promise -> String
promiseToString = fmap C.toLower . show
