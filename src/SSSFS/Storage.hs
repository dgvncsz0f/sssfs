{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module SSSFS.Storage
       ( -- | Types
         Ref(..)
       , Loc
       , Payload
       , Storage(..)
         -- | Ref/Loc Functions
       , ref
       , loc
       , loc2
       , showLoc
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

newtype Ref   = Ref { unRef :: T.Text }
              deriving (Eq,Ord,Show)

type Loc      = [Ref]

type Payload  = B.ByteString

showLoc :: Loc -> T.Text
showLoc = ("/" `T.append`) . T.intercalate "/" . map unRef

ref :: String -> Ref
ref = Ref . T.pack

loc :: Ref -> Loc
loc = (:[])

loc2 :: String -> Loc
loc2 = (:[]) . ref
           
-- | The primitives that every storage needs to supply.
class Storage s where
  
  -- | Writes content and makes it available under a given location.
  put  :: s -> Loc -> Payload -> IO ()
  
  -- | Returns the latest payload information associated with a given
  -- location.
  get  :: s -> Loc -> IO Payload
  
  -- | Checks whether or a not a given resource is available.
  stat :: s -> Loc -> IO Bool
  
  -- | Enumerates all locations that are proper prefixes of a given
  -- location. If you consider locations hierarchically, then a proper
  -- prefix will be the all of its proper children. For instance:
  -- 
  -- Suppose [foo,bar,baz] and [foo,baz,bar] are both defined, then
  -- enum [foo] = [bar,baz].
  -- 
  -- There is no well defined ordering for the results.
  enum :: s -> Loc -> IO [Loc]

enumKey :: (MonadIO m, Storage s) => s -> Loc -> Onum B.ByteString m ()
enumKey s l = mkInumC id noCtl (liftIO $ get s l)

computeHash :: B.ByteString -> Loc
computeHash = loc2 . render . hexdumpBy "" 64 . hash . B.unpack

instance IsString Ref where
  fromString = Ref . T.pack

instance S.Serialize Ref where
  
  put = S.put . encodeUtf8 . unRef
  get = fmap (Ref . decodeUtf8) S.get