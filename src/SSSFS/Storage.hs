{-# LANGUAGE OverloadedStrings #-}

module SSSFS.Storage
       ( -- | Types
         Ref(..)
       , Key
       , Payload
       , Storage(..)
         -- | Ref/Key Functions
       , ref
       , fromRef
       , fromStr
       , showRefS
       , showKey
       , showKeyS
         -- | Iteratee
       , enumKey
         -- | Misc
       , computeHash
       ) where

import           Codec.Text.Raw
import           Control.Monad.IO.Class
import           Data.IterIO
import qualified Data.Serialize as S
import qualified Data.ByteString as B
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.Digest.SHA256
import           Text.PrettyPrint.HughesPJ (render)
import           GHC.Exts

newtype Ref   = Ref { showRef :: T.Text }
              deriving (Eq,Ord,Show)

type Key      = [Ref]

type Payload  = B.ByteString

showKey :: Key -> T.Text
showKey = ("/" `T.append`) . T.intercalate "/" . map showRef

showKeyS :: Key -> String
showKeyS = T.unpack . showKey

showRefS :: Ref -> String
showRefS = T.unpack . showRef

ref :: String -> Ref
ref = Ref . T.pack

fromRef :: Ref -> Key
fromRef = (:[])

fromStr :: String -> Key
fromStr = (:[]) . ref
           
-- | The primitives that every storage needs to supply.
class Storage s where
  
  -- | Writes content and makes it available under a given location.
  put  :: s -> Key -> Payload -> IO ()
  
  -- | Returns the latest payload information associated with a given
  -- location.
  get  :: s -> Key -> IO Payload
  
  -- | Deletes a given resource
  del  :: s -> Key -> IO ()
  
  -- | Checks whether or a not a given resource is available.
  head :: s -> Key -> IO Bool
  
  -- | Enumerates all locations that are proper prefixes of a given
  -- location. If you consider locations hierarchically, then a proper
  -- prefix will be the all of its proper children. For instance:
  -- 
  -- Suppose [foo,bar,baz] and [foo,baz,bar] are both defined, then
  -- enum [foo] = [bar,baz].
  -- 
  -- There is no well defined ordering for the results.
  enum :: s -> Key -> IO [Ref]

-- | Provides an enumerator for the contents of a given key.
enumKey :: (MonadIO m, Storage s) => s -> Key -> Onum B.ByteString m ()
enumKey s l = mkInumC id noCtl (liftIO $ get s l)

-- | Currently computes the sha256 of a given bytestring.
computeHash :: B.ByteString -> Key
computeHash = fromStr . render . hexdumpBy "" 64 . hash . B.unpack

instance IsString Ref where
  fromString = Ref . T.pack

instance S.Serialize Ref where
  
  put = S.put . encodeUtf8 . showRef
  get = fmap (Ref . decodeUtf8) S.get