{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE QualifiedDo       #-}
module Main where

import           Control.Monad.IO.Class
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           System.Directory           (getDirectoryContents)

import           System.OpenBSD.MultiPledge as M
import           System.OpenBSD.Pledge      (Promise (..))

pledgePutStrLn :: (MonadIO m) => T.Text -> Pledge m zs '[ 'Stdio ] ()
pledgePutStrLn = Pledge . liftIO . T.putStrLn

pledgeGetDirectoryContents :: (MonadIO m) => FilePath -> Pledge m zs '[ 'Rpath ] [FilePath]
pledgeGetDirectoryContents = Pledge . liftIO . getDirectoryContents

pledgeGetLine :: (MonadIO m) => Pledge m zs '[ 'Stdio] T.Text
pledgeGetLine = Pledge $ liftIO $ T.getLine

main :: IO ()
main = runPledge $ M.do
  l <- pledgeGetLine
  fs <- fmap (fmap T.pack) $ pledgeGetDirectoryContents "/"
  pledgePutStrLn $ "hello " <> l
  fmap mconcat $ traverse pledgePutStrLn fs
