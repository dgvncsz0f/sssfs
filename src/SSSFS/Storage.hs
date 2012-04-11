{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

-- Copyright (c) 2012, Diego Souza
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
-- 
--   * Redistributions of source code must retain the above copyright notice,
--     this list of conditions and the following disclaimer.
--   * Redistributions in binary form must reproduce the above copyright notice,
--     this list of conditions and the following disclaimer in the documentation
--     and/or other materials provided with the distribution.
--   * Neither the name of the <ORGANIZATION> nor the names of its contributors
--     may be used to endorse or promote products derived from this software
--     without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


module SSSFS.Storage
       ( -- | Types
         Ref(..)
       , Key
       , Payload
       , StorageHashLike(..)
       , StorageEnumLike(..)
       , StorageContext(..)
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

class StorageHashLike s where
  
  -- | Writes content and makes it available under a given location.
  put  :: s -> Key -> Payload -> IO ()
  
  -- | Returns the latest payload information associated with a given
  -- location.
  get  :: s -> Key -> IO Payload
  
  -- | Deletes a given resource
  del  :: s -> Key -> IO ()
  
  -- | Checks whether or a not a given resource is available.
  head :: s -> Key -> IO Bool
  
class StorageEnumLike s where
  -- | Enumerates all locations that are proper prefixes of a given
  -- location. If you consider locations hierarchically, then a proper
  -- prefix will be the all of its proper children. For instance:
  -- 
  -- Suppose [foo,bar,baz] and [foo,baz,bar] are both defined, then
  -- enum [foo] = [bar,baz].
  -- 
  -- There is no well defined ordering for the results.
  enumKeys :: s -> Key -> IO [Ref]
  
  enumTest :: s -> Key -> IO Bool
  enumTest s k = fmap (not . null) (enumKeys s k)

class (StorageHashLike s, StorageEnumLike s) => StorageContext s r where
  
  enum :: s -> Key -> IO r

-- | Provides an enumerator for the contents of a given key.
enumKey :: (MonadIO m, StorageHashLike s) => s -> Key -> Onum B.ByteString m ()
enumKey s l = mkInumC id noCtl (liftIO $ get s l)

-- | Currently computes the sha256 of a given bytestring.
computeHash :: B.ByteString -> Key
computeHash = fromStr . render . hexdumpBy "" 64 . hash . B.unpack

instance IsString Ref where
  fromString = Ref . T.pack

instance S.Serialize Ref where
  
  put = S.put . encodeUtf8 . showRef
  get = fmap (Ref . decodeUtf8) S.get

instance (StorageHashLike s, StorageEnumLike s) => StorageContext s Bool where
  
  enum s k = enumTest s k

instance (StorageHashLike s, StorageEnumLike s) => StorageContext s ([] Ref) where
  
  enum s k = enumKeys s k

