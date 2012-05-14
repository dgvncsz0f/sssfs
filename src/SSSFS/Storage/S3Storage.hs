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


module SSSFS.Storage.S3Storage
       ( S3Storage(..)
       , amazonS3Connection
       ) where

import           Control.Exception
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import           Network.AWS.AWSConnection
import           Network.AWS.AWSResult
import           Network.AWS.S3Bucket
import           Network.AWS.S3Object
import           SSSFS.Except
import           SSSFS.Storage

data S3Storage = S3Storage { conn   :: AWSConnection 
                           , bucket :: String
                           }

toURI :: Key -> String
toURI = tail . showKeyS

toObject :: S3Storage -> Key -> B.ByteString -> S3Object
toObject s k v = setStorageClass REDUCED_REDUNDANCY o
  where o = S3Object (bucket s) (toURI k) ("application/octet-stream") [] (toLazy v)

toLazy :: B.ByteString -> L.ByteString
toLazy = L.fromChunks . (:[])

fromLazy :: L.ByteString -> B.ByteString
fromLazy = B.concat . L.toChunks

-- | TODO: deal with redirect errors
unpackResult :: Either ReqError a -> a
unpackResult (Left (AWSError "NotFound" m))  = throw (NotFound m)
unpackResult (Left (AWSError "NoSuchKey" m)) = throw (NotFound m)
unpackResult (Left e)                        = throw (SysExcept $ show e)
unpackResult (Right a)                       = a

instance Storage S3Storage

instance StorageHashLike S3Storage where
  
  put s k v = fmap unpackResult (sendObject (conn s) o)
    where o = toObject s k v
  
  get s k = fmap (fromLazy . obj_data . unpackResult) (getObject (conn s) o)
    where o = toObject s k B.empty
  
  head s k = do { result <- getObjectInfo (conn s) o
                ; case result
                  of Right _
                       -> return True
                     Left (AWSError "NoSuchKey" _)
                       -> return False
                     Left (AWSError "NotFound" _)
                       -> return False
                     Left e
                       -> throw (SysExcept $ show e)
                }
    where o = toObject s k B.empty
  
  del s k = fmap unpackResult (deleteObject (conn s) o)
    where o = toObject s k B.empty

instance StorageEnumLike S3Storage where
  
  enumKeys s k = do { objects <- fmap unpackResult (listAllObjects (conn s) (bucket s) s3Filter)
                    ; return (map makeRef objects)
                    }
    where s3Filter =  ListRequest uriPrefix "" "/" 1000

          uriPrefix = toURI k ++ "/"
          
          makeRef = ref . drop (length uriPrefix) . key