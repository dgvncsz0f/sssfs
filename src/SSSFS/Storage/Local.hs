{-# LANGUAGE OverloadedStrings #-}

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


-- | This module provides a very simple in-memory implementation of a
-- backend. You generally don't want to use this, except perhaps for
-- testing purposes.
-- TODO: handle exceptions properly
module SSSFS.Storage.Local
       ( new
       ) where

import           Control.Exception as C
import qualified Data.ByteString as B
import qualified Data.Text as T
import           Data.List (isPrefixOf)
import           System.Directory
import           System.FilePath
import           System.IO.Error
import           System.IO
import           SSSFS.Except
import           SSSFS.Storage

newtype LocalStorage = LocalStorage FilePath

maxSize :: Int
maxSize = 64 * 1024

new :: FilePath -> LocalStorage
new = LocalStorage

-- | Relies on POSIX guarantees that a move operation in the same
-- partition is atomic. You better be using a local filesystem.
atomicWrite :: FilePath -> B.ByteString -> IO ()
atomicWrite dst contents = let dir = takeDirectory dst
                           in do { (tmpF, tmpH) <- openBinaryTempFile dir "x-write.XXX"
                                 ; B.hPut tmpH contents
                                 ; hClose tmpH
                                 ; renameFile tmpF dst
                                 }

readContent :: FilePath -> IO B.ByteString
readContent file = do { h <- openBinaryFile file ReadMode
                      ; r <- B.hGetSome h maxSize
                      ; hClose h
                      ; return r
                      }

encodePath :: FilePath -> FilePath
encodePath = joinPath . rewrite . splitPath
  where rewrite []     = []
        rewrite [s]    = ['f' : s]
        rewrite (x:xs) = ('d' : x) : rewrite xs

encodePathDir :: FilePath -> FilePath
encodePathDir = joinPath . map ('d':) . splitPath

decodePath :: FilePath -> FilePath
decodePath = tail

chroot :: FilePath -> Key -> (FilePath -> FilePath) -> FilePath
chroot root l f
  | isAbsolute path = root </> (f $ dropWhile (`elem` pathSeparators) path)
  | otherwise       = root </> (f path)
    where path = T.unpack $ T.intercalate "/" (map showRef l)

chroot_ :: FilePath -> Key -> FilePath
chroot_ root l = chroot root l encodePath

instance StorageHashLike LocalStorage where
  
  put (LocalStorage root) k v = do { createDirectoryIfMissing True (chroot root k encodePathDir)
                                   ; atomicWrite (chroot_ root k) v
                                   }
  
  get (LocalStorage root) k = C.catch (readContent (chroot_ root k))
                                      (\e -> if (isDoesNotExistError e)
                                             then (throw $ NotFound (showKeyS k))
                                             else ioError e)
  
  del (LocalStorage root) k = removeFile (chroot_ root k)
  
  head (LocalStorage root) k = doesFileExist (chroot_ root k)
  
instance StorageEnumLike LocalStorage where
  
  enumKeys (LocalStorage root) k = let path       = chroot root k encodePathDir
                                       decode     = map (ref . decodePath)
                                       filterList = filter ("f" `isPrefixOf`)
                                   in fmap (decode . filterList) (getDirectoryContents path)
