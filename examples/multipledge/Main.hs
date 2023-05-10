{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE QualifiedDo       #-}
module Main where

import           Control.Monad.IO.Class
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Prelude                    hiding (getLine, putStrLn)
-- ^ Hiding these because we want to reuse their names.
import qualified System.Directory           as D (getDirectoryContents)

import           Data.Set.Singletons
import           Data.Singletons
import           System.OpenBSD.MultiPledge as M
import           System.OpenBSD.Pledge      (Promise (..))

putStrLn :: (MonadIO m) => T.Text -> Pledge zs '[ 'Stdio] m ()
putStrLn = liftIO . T.putStrLn

getDirectoryContents :: (MonadIO m) => FilePath -> Pledge zs '[ 'Rpath] m [FilePath]
getDirectoryContents = liftIO . D.getDirectoryContents

getLine :: (MonadIO m) => Pledge zs '[ 'Stdio] m T.Text
getLine = liftIO $ T.getLine

helloGoodbye :: ( MonadIO m
                , SingI zs
                )
             => Pledge zs '[ 'Stdio] m ()
helloGoodbye = do -- We can use ordinary monadic do here because both
                  -- actions use the same promises. This saves an
                  -- unnecessary pledge call.
  l <- getLine
  putStrLn l

main' :: ( MonadIO m
         , SingI zs
         )
      => Pledge zs (AsSet '[ 'Stdio, 'Rpath]) m ()
main' = M.do
  l <- getLine
  fs <- fmap (fmap T.pack) $ getDirectoryContents "/"
  do
    putStrLn $ "hello " <> l
    fmap mconcat $ traverse putStrLn fs

main :: IO ()
main = runPledge main'
