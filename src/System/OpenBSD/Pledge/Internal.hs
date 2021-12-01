module System.OpenBSD.Pledge.Internal (Promise(..), pledge) where

import           System.OpenBSD.Pledge.Promise.Type (Promise (..),
                                                     promiseToString)

import           Control.Monad                      (void)
import           Control.Monad.IO.Class             (MonadIO, liftIO)
import           Data.List                          (intercalate)
import           Data.Set                           (Set)
import qualified Data.Set                           as S (toList)
import           Foreign.C.String                   (CString, newCString)

empty :: IO CString
empty = newCString ""

-- TODO define pledge constants as byte strings
-- figure out how to turn those into cstrings

pledge :: (MonadIO m) => Set Promise -> m ()
pledge = liftIO . pledgeString . intercalate " " . fmap promiseToString . S.toList

-- | The core function applying pledges
pledgeString :: String -> IO ()
pledgeString p = do
  promises <- newCString p
  empty' <- empty
  void $ c_pledge promises empty'

foreign import ccall unsafe "pledge"
  c_pledge :: CString -> CString -> IO Int
