{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
module Main where

import Prelude hiding ((>>),(>>=), return, putStrLn)

import System.OpenBSD.Pledge (runPledge, Promise(..), Pledge(..))

import qualified System.Environment as E (getArgs)
import Control.Effect
import Data.String (fromString)
import qualified Data.Text.IO as T (readFile, putStrLn)
import Data.Text (Text, pack)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Foldable (traverse_)
import qualified System.Directory as D (getDirectoryContents)

putStrLn :: (MonadIO m) => Text -> Pledge m '[ 'Stdio ] ()
putStrLn = Pledge . liftIO . T.putStrLn

getDirectoryContents :: (MonadIO m) => FilePath -> Pledge m '[ 'Rpath ] [FilePath]
getDirectoryContents = Pledge . liftIO . D.getDirectoryContents

readFile :: (MonadIO m) => FilePath -> Pledge m '[ 'Stdio ] Text
readFile = Pledge . liftIO . T.readFile 

getArgs :: (MonadIO m) => Pledge m '[ ] [Text]
getArgs = Pledge $ liftIO $ fmap (fmap pack) $ E.getArgs

main :: IO ()
main = runPledge $ do
  fn <- getArgs
  putStrLn "hello"
  traverse_ putStrLn fn
  d <- getDirectoryContents "."
  traverse_ (putStrLn . pack) d
