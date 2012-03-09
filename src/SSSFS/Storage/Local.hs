{-# LANGUAGE OverloadedStrings #-}

-- TODO: handle exceptions properly

-- | This module provides a very simple in-memory implementation of a
-- backend. You generally don't want to use this, except perhaps for
-- testing purposes.
module SSSFS.Storage.Local
       ( new
       ) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import           System.Directory
import           System.FilePath
import           System.IO
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
                           in do { createDirectoryIfMissing True dir
                                 ; (tmpF, tmpH) <- openBinaryTempFile dir "x-write.XXX"
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

instance Storage LocalStorage where
  
  put (LocalStorage root) k v = atomicWrite (chroot_ root k) v
  
  get (LocalStorage root) k = readContent (chroot_ root k)
  
  del (LocalStorage root) k = removeFile (chroot_ root k)
  
  head (LocalStorage root) k = doesFileExist (chroot_ root k)
  
  enum (LocalStorage root) k = let path       = chroot root k encodePathDir
                                   decode     = map (ref . decodePath)
                                   filterList = filter (`notElem` [".",".."])
                               in fmap (decode . filterList) (getDirectoryContents path)
