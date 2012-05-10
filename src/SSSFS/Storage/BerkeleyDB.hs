{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE EmptyDataDecls           #-}

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


module SSSFS.Storage.BerkeleyDB
       ( BdbStorage()
       , new
       ) where

import           Control.Exception
import           Foreign hiding (new)
import           Foreign.C
import qualified Data.ByteString as B
import           SSSFS.Except
import           SSSFS.Storage

data CStorage
type CStoragePtr = Ptr CStorage
data Result = ROk
            | RNotFound
            | RError
            | Rc Int
            deriving (Eq)

data BdbStorage = BdbStorage { unPtr :: CStoragePtr }

withStorage :: BdbStorage -> (CStoragePtr -> IO CInt) -> IO Result
withStorage s f = f (unPtr s) >>= checkRc
  where checkRc rc
          | rc == 0    = return ROk
          | rc == (-2) = return RNotFound
          | rc == (-1) = return RError
          | otherwise  = return $ Rc (fromIntegral rc)

cStr :: CStringLen -> CString
cStr = fst

cStrLen :: CStringLen -> CInt
cStrLen = fromIntegral . snd

failure :: a
failure = throw (SysExcept "system failure")

notFound :: String -> a
notFound = throw . NotFound

new :: FilePath -> IO BdbStorage
new file = withCString file $ \cstr -> fmap BdbStorage (cstorage_init cstr)

destroy :: BdbStorage -> IO ()
destroy = cstorage_destroy . unPtr

instance Storage BdbStorage where
  
  shutdown = destroy

instance StorageHashLike BdbStorage where
  
  put s k v = hstorage_put s (showKeyS k) v

  head s k = hstorage_has s (showKeyS k)

  del s k = hstorage_del s (showKeyS k)
  
  get s k = hstorage_get s (showKeyS k)

foreign import ccall safe "bdb_storage.h storage_init"
  cstorage_init :: CString -> IO CStoragePtr

foreign import ccall safe "bdb_storage.h storage_destroy"
  cstorage_destroy :: CStoragePtr -> IO ()

foreign import ccall safe "bdb_storage.h storage_put"
  cstorage_put :: CStoragePtr -> CString -> CInt -> CString -> CInt -> IO CInt

hstorage_put :: BdbStorage -> String -> B.ByteString -> IO ()
hstorage_put s k0 v0 = withCStringLen k0    $ \k ->
                       B.useAsCStringLen v0 $ \v ->
                         fmap check (withStorage s $ \cs -> cstorage_put cs (cStr k) (cStrLen k) (cStr v) (cStrLen v))
  where check ROk = ()
        check _   = failure

foreign import ccall safe "bdb_storage.h storage_has"
  cstorage_has :: CStoragePtr -> CString -> CInt -> IO CInt

hstorage_has :: BdbStorage -> String -> IO Bool
hstorage_has s k0 = withCStringLen k0 $ \k ->
                      fmap getResult (withStorage s $ \cs -> cstorage_has cs (cStr k) (cStrLen k))
  where getResult ROk       = True
        getResult RNotFound = False
        getResult _         = failure

foreign import ccall safe "bdb_storage.h storage_del"
  cstorage_del :: CStoragePtr -> CString -> CInt -> IO CInt

hstorage_del :: BdbStorage -> String -> IO ()
hstorage_del s k0 = withCStringLen k0 $ \k ->
                      fmap check (withStorage s $ \cs -> cstorage_del cs (cStr k) (cStrLen k))
  where check ROk       = ()
        check RNotFound = ()
        check _         = failure

foreign import ccall safe "bdb_storage.h storage_get"
  cstorage_get :: CStoragePtr -> CString -> CInt -> CString -> CInt -> IO CInt

hstorage_get :: BdbStorage -> String -> IO B.ByteString
hstorage_get s k0 = withCStringLen k0 $ \k ->
                      do { sz <- fmap getSize (withStorage s $ \cs -> cstorage_get cs (cStr k) (cStrLen k) nullPtr 0)
                         ; allocaBytes sz $ \cval -> 
                            do { fmap check (withStorage s $ \cs -> cstorage_get cs (cStr k) (cStrLen k) cval (fromIntegral sz))
                               ; B.packCStringLen (cval, sz)
                               }
                         }
  where getSize (Rc sz)   = sz
        getSize RError    = failure
        getSize ROk       = failure
        getSize RNotFound = notFound k0
        
        check ROk = ()
        check _   = failure
