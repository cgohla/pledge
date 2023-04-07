module Main where

import           Control.Monad                  (void)
import qualified Data.Set                       as S (fromList)
import           System.Environment             (getArgs)
import           System.OpenBSD.Pledge.Internal (Promise (..), pledge)

main :: IO ()
main = do
  putStrLn "pledge demo"
  args <- getArgs
  case args of
    ("--pledge":_) -> do
      let promises = S.fromList [Stdio]
      putStrLn "with pledge"
      putStrLn $ "applying promises: " ++ show promises
      pledge promises
    _ -> putStrLn "without pledge"
  putStrLn "try to open a file. should fail when running with pledge."
  void $ readFile "/tmp/demo.dummy.foo"

