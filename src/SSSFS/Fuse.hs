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

module SSSFS.Fuse
       ( sssfsMain
       ) where

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
import           SSSFS.Storage as S
import           SSSFS.Filesystem.Types as T
import           SSSFS.Filesystem.Core
import           SSSFS.Filesystem.Files
import           SSSFS.Filesystem.Directory

type FHandle = IORef FileHandle

itypeToEntryType :: IType -> EntryType
itypeToEntryType File        = RegularFile
itypeToEntryType T.Directory = F.Directory

epochTimeToTimestamp :: EpochTime -> Timestamp
epochTimeToTimestamp x = (i, 0)
  where i = read $ show x

inodeToFileStat :: INode -> FileStat
inodeToFileStat inum = FileStat { statEntryType        = itypeToEntryType (itype inum)
                                , statFileMode         = fmode
                                , statLinkCount        = 1
                                , statFileOwner        = 1000
                                , statFileGroup        = 1000
                                , statSpecialDeviceID  = 0
                                , statFileSize         = fromIntegral (size inum)
                                , statBlocks           = fromIntegral (numBlocks * blkFactor)
                                , statAccessTime       = fromIntegral (fst $ atime inum)
                                , statModificationTime = fromIntegral (fst $ mtime inum)
                                , statStatusChangeTime = fromIntegral (fst $ ctime inum)
                                }
  where fmode = case (itype inum)
                of File 
                     -> ownerReadMode .|. ownerWriteMode
                   T.Directory 
                     -> ownerReadMode .|. ownerWriteMode .|. ownerExecuteMode
        
        numBlocks = fromIntegral $ snd (blocks inum)
        blkFactor = blksz inum `div` 512

install :: IO a -> (Errno -> IO a) -> IO a
install f g = f `catches` [ Handler (\(e :: IOExcept)      -> handlerA e)
                          , Handler (\(e :: SysExcept)     -> handlerB e)
                          , Handler (\(e :: SomeException) -> handlerB e)
                          ]
  where handlerA (NotFound _) = g eNOENT
        handlerA (NotADir _)  = g eNOTDIR
        handlerA (NotEmpty _) = g eNOTEMPTY
        handlerA (IsDir _)    = g eISDIR
        
        handlerB e = print e >> g eFAULT

exToEither :: IO a -> IO (Either Errno a)
exToEither f = install (fmap Right f) (return . Left)

exToErrno :: IO () -> IO Errno
exToErrno f = install (f >> return eOK) (return)

fsMknod :: (StorageHashLike s) => s -> FilePath -> EntryType -> FileMode -> DeviceID -> IO Errno
fsMknod s path RegularFile _ _ = exToErrno (creat s path >> return ())
fsMknod _ _ _ _ _              = return eNOSYS

fsMkdir :: (StorageHashLike s) => s -> FilePath -> FileMode -> IO Errno
fsMkdir s path _ = exToErrno $ mkdir s path >> return ()

fsReadDir :: (StorageHashLike s, StorageEnumLike s) => s -> FilePath -> IO (Either Errno [(FilePath, FileStat)])
fsReadDir s path = exToEither f
  where f = fmap (map (\(p, i) -> (p, inodeToFileStat i))) (readDir s path)

fsRmdir :: (StorageHashLike s, StorageEnumLike s) => s -> FilePath -> IO Errno
fsRmdir s path = exToErrno $ rmdir s path

fsStat :: (StorageHashLike s) => s -> FilePath -> Maybe FHandle -> IO (Either Errno FileStat)
fsStat s path Nothing = exToEither (fmap inodeToFileStat (stat s path))
fsStat s _ (Just rfh) = exToEither (fmap inodeToFileStat (readIORef rfh >>= fstat s))

fsOpen :: (StorageHashLike s) => s -> FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno FHandle)
fsOpen s path _ flags = exToEither $ sysopen >>= fsync s >>= newIORef
  where sysopen
          | trunc flags = open s path >>= flip (ftruncate s) 0
          | otherwise   = open s path

fsInit :: (StorageHashLike s) => s -> FuseConnInfo -> IO FuseConnInfo
fsInit s conn = do { oldfs <- S.head s keyOne 
                   ; when (not oldfs) (mkfs s blockSize)
                   ; return (FuseConnSet { asyncRead    = True
                                         , maxWrite     = blockSize
                                         , maxReadAhead = maxReadAhead conn
                                         , want = [ FuseCapBigWrites
                                                  , FuseCapAtomicOTrunc
                                                  ]
                                         })
                   }
  where blockSize = min (512*1024) (maxWrite conn)

fsRead :: (StorageHashLike s) => s -> FilePath -> FHandle -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
fsRead s _ rfh pSize pOffset = exToEither $ readIORef rfh >>= sysread
  where offset = fromIntegral pOffset
        
        bufsz = fromIntegral pSize
        
        sysread fh = fmap snd (fread s fh offset bufsz)

fsWrite :: (StorageHashLike s) => s -> FilePath -> FHandle -> B.ByteString -> FileOffset -> IO (Either Errno ByteCount)
fsWrite s _ rfh bytes pOffset = exToEither $ do { fh <- readIORef rfh >>= syswrite
                                                ; _  <- modifyIORef rfh (const fh)
                                                -- ; fsync s fh
                                                ; return (fromIntegral $ B.length bytes)
                                                }
  where syswrite fh = fwrite s fh bytes (fromIntegral pOffset)

fsTruncate :: (StorageHashLike s) => s -> FilePath -> FileOffset -> Maybe FHandle -> IO Errno
fsTruncate s path pOffset Nothing = exToErrno $ do { fh <- open s path
                                                   ; _  <- ftruncate s fh (fromIntegral pOffset) >>= fsync s
                                                   ; return ()
                                                   }
fsTruncate s _ pOffset (Just rfh) = exToErrno $ do { fh <- readIORef rfh >>= flip (ftruncate s) (fromIntegral pOffset)
                                                   ; _  <- modifyIORef rfh (const fh)
                                                   ; return ()
                                                   }

fsUtime :: (StorageHashLike s) => s -> FilePath -> EpochTime -> EpochTime -> IO Errno
fsUtime s path uAtime0 uMtime0 = exToErrno $ utime s path uAtime uMtime
  where uAtime = epochTimeToTimestamp uAtime0
        uMtime = epochTimeToTimestamp uMtime0

fsUnlink :: (StorageHashLike s) => s -> FilePath -> IO Errno
fsUnlink s path = exToErrno $ unlink s path

fsFlush :: (StorageHashLike s) => s -> FilePath -> FHandle -> IO Errno
fsFlush _ _ _ = return eOK

fsFSync :: (StorageHashLike s) => s -> FilePath -> SyncType -> FHandle -> IO Errno
fsFSync s _ _ rfh = exToErrno $ do { fh <- readIORef rfh >>= fsync s
                                   ; _  <- modifyIORef rfh (const fh)
                                   ; return ()
                                   }

fsRelease :: (StorageHashLike s) => s -> FilePath -> FHandle -> IO ()
fsRelease s _ rfh = do { _ <- readIORef rfh >>= fsync s
                       ; return ()
                       }

sssfs :: (Storage s, StorageHashLike s, StorageEnumLike s) => s -> FuseOperations FHandle
sssfs s =  FuseOperations { fuseGetFileStat          = fsStat s
                          , fuseReadSymbolicLink     = \_ -> return (Left eNOSYS)
                          , fuseCreateDevice         = fsMknod s
                          , fuseCreateDirectory      = fsMkdir s
                          , fuseReadDirectory        = fsReadDir s
                          , fuseRemoveLink           = fsUnlink s
                          , fuseRemoveDirectory      = fsRmdir s
                          , fuseCreateSymbolicLink   = \_ _ -> return eNOSYS
                          , fuseRename               = \_ _ -> return eNOSYS
                          , fuseCreateLink           = \_ _ -> return eNOSYS
                          , fuseSetFileMode          = \_ _ -> return eOK
                          , fuseSetOwnerAndGroup     = \_ _ _ -> return eOK
                          , fuseSetFileSize          = fsTruncate s
                          , fuseSetFileTimes         = fsUtime s
                          , fuseOpen                 = fsOpen s
                          , fuseRead                 = fsRead s
                          , fuseWrite                = fsWrite s
                          , fuseGetFileSystemStats   = \_ -> return (Left eNOSYS)
                          , fuseFlush                = fsFlush s
                          , fuseRelease              = fsRelease s
                          , fuseSynchronizeFile      = fsFSync s
                          , fuseOpenDirectory        = \_ -> return eOK
                          , fuseReleaseDirectory     = \_ -> return eOK
                          , fuseSynchronizeDirectory = \_ _ -> return eOK
                          , fuseAccess               = \_ _ -> return eOK
                          , fuseInit                 = fsInit s
                          , fuseDestroy              = shutdown s
                          }

sssfsMain :: (StorageHashLike s, StorageEnumLike s) => s -> (FuseOperations FHandle -> FuseOperations FHandle) -> IO ()
sssfsMain s f = fuseMain (f $ sssfs s) defaultExceptionHandler
