{-# LANGUAGE ScopedTypeVariables #-}

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

module SSSFS.Fuse where

import           Control.Monad
import           Control.Exception
import           Data.Bits
import           Data.IORef
import           Foreign.C.Error
import           System.Fuse as F
import           System.Posix.Types
import           System.Posix.Files
import qualified Data.ByteString as B
import           SSSFS.Except
import           SSSFS.Fuse.Debug
import           SSSFS.Storage as S
import           SSSFS.Filesystem.Types as T
import           SSSFS.Filesystem.Core
import           SSSFS.Filesystem.Files
import           SSSFS.Filesystem.Directory

type FHandle = IORef FileHandle

itypeToEntryType :: IType -> EntryType
itypeToEntryType File        = RegularFile
itypeToEntryType T.Directory = F.Directory

inodeToFileStat :: INode -> FileStat
inodeToFileStat inum = FileStat { statEntryType        = itypeToEntryType (itype inum)
                                , statFileMode         = fmode
                                , statLinkCount        = 1
                                , statFileOwner        = 1000
                                , statFileGroup        = 1000
                                , statSpecialDeviceID  = 0
                                , statFileSize         = fromInteger (size inum)
                                , statBlocks           = 0
                                , statAccessTime       = fromIntegral (fst $ atime inum)
                                , statModificationTime = fromIntegral (fst $ mtime inum)
                                , statStatusChangeTime = fromIntegral (fst $ ctime inum)
                                }
  where fmode = case (itype inum)
                of File 
                     -> ownerReadMode .|. ownerWriteMode
                   T.Directory 
                     -> ownerReadMode .|. ownerWriteMode .|. ownerExecuteMode

install :: IO a -> (Errno -> IO a) -> IO a
install f g = f `catches` [ Handler (\(e :: IOExcept)      -> handlerA e)
                          , Handler (\(e :: SysExcept)     -> handlerB e)
                          , Handler (\(e :: SomeException) -> handlerB e)
                          ]
  where handlerA (NotFound _) = g eNOENT
        handlerA (NotADir _)  = g eNOTDIR
        handlerA (NotEmpty _) = g eNOTEMPTY
        handlerA (IsDir _)   = g eISDIR
        
        handlerB _ = g eFAULT

exToEither :: IO a -> IO (Either Errno a)
exToEither f = install (fmap Right f) (return . Left)

exToErrno :: IO () -> IO Errno
exToErrno f = install (f >> return eOK) (return)

fsMknod :: (StorageHashLike s) => s -> FilePath -> EntryType -> FileMode -> DeviceID -> IO Errno
fsMknod s path RegularFile _ _ = exToErrno (creat s path >> return ())
fsMknod _ _ _ _ _              = return eFAULT

fsMkdir :: (StorageHashLike s) => s -> FilePath -> FileMode -> IO Errno
fsMkdir s path _ = exToErrno f
  where f = mkdir s path >> return ()

fsReadDir :: (StorageHashLike s, StorageEnumLike s) => s -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
fsReadDir s path = exToEither f
  where f = fmap (map (\(p, i) -> (p, inodeToFileStat i))) (readDir s path)

fsRmdir :: (StorageHashLike s, StorageEnumLike s) => s -> FilePath -> IO Errno
fsRmdir s path = exToErrno f
  where f = rmdir s path

fsStat :: (StorageHashLike s) => s -> FilePath -> IO (Either Errno FileStat)
fsStat s path = exToEither f
  where f = fmap inodeToFileStat (stat s path)

fsOpen :: (StorageHashLike s) => s -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno FHandle)
fsOpen s path _ _ = exToEither f
  where f = do { fh <- open s path
               ; newIORef fh
               }

fsInit :: (StorageHashLike s) => s -> IO ()
fsInit s = do { oldfs <- S.head s keyOne 
              ; when (not oldfs) (mkfs s)
              }

fsRead :: (StorageHashLike s) => s -> FilePath -> FHandle -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
fsRead s _ rfh pSize pOffset = readIORef rfh >>= sysread
  where offset = fromIntegral pOffset
        
        bufsz = fromIntegral pSize
        
        sysread fh = fmap Right (fread s fh offset bufsz)

fsWrite :: (StorageHashLike s) => s -> FilePath -> FHandle -> B.ByteString -> FileOffset -> IO (Either Errno ByteCount)
fsWrite s _ rfh bytes pOffset = do { fh <- readIORef rfh >>= syswrite
                                   ; _  <- modifyIORef rfh (const fh)
                                   ; return (Right $ fromIntegral $ B.length bytes)
                                   }
  where syswrite fh = fwrite s fh bytes (fromIntegral pOffset)

fsTruncate :: (StorageHashLike s) => s -> FilePath -> FileOffset -> IO Errno
fsTruncate s path pOffset = do { fh <- open s path
                               ; ftruncate s fh offset >>= fsync s
                               ; return eOK
                               }
  where offset = fromIntegral pOffset

fsFlush :: (StorageHashLike s) => s -> FilePath -> FHandle -> IO Errno
fsFlush s _ rfh = readIORef rfh >>= fsync s >> return eOK

fsRelease :: (StorageHashLike s) => s -> FilePath -> FHandle -> IO ()
fsRelease s _ rfh = readIORef rfh >>= fsync s

fuseOps :: (StorageHashLike s, StorageEnumLike s) => s -> FuseOperations FHandle
fuseOps s =  FuseOperations { fuseGetFileStat          = fsStat s
                            , fuseReadSymbolicLink     = \_ -> return (Left eFAULT)
                            , fuseCreateDevice         = fsMknod s
                            , fuseCreateDirectory      = fsMkdir s
                            , fuseReadDirectory        = fsReadDir s
                            , fuseRemoveLink           = \_ -> return eFAULT
                            , fuseRemoveDirectory      = fsRmdir s
                            , fuseCreateSymbolicLink   = \_ _ -> return eFAULT
                            , fuseRename               = \_ _ -> return eFAULT
                            , fuseCreateLink           = \_ _ -> return eFAULT
                            , fuseSetFileMode          = \_ _ -> return eOK
                            , fuseSetOwnerAndGroup     = \_ _ _ -> return eOK
                            , fuseSetFileSize          = fsTruncate s
                            , fuseSetFileTimes         = \_ _ _ -> return eOK
                            , fuseOpen                 = fsOpen s
                            , fuseRead                 = fsRead s
                            , fuseWrite                = fsWrite s
                            , fuseGetFileSystemStats   = \_ -> return (Left eFAULT)
                            , fuseFlush                = fsFlush s
                            , fuseRelease              = fsRelease s
                            , fuseSynchronizeFile      = \_ _ -> return eOK
                            , fuseOpenDirectory        = \_ -> return eOK
                            , fuseReleaseDirectory     = \_ -> return eOK
                            , fuseSynchronizeDirectory = \_ _ -> return eOK
                            , fuseAccess               = \_ _ -> return eOK
                            , fuseInit                 = fsInit s
                            , fuseDestroy              = return ()
                            }

main :: (StorageHashLike s, StorageEnumLike s) => s -> IO ()
main s = fuseMain (debugFuse (fuseOps s)) defaultExceptionHandler
