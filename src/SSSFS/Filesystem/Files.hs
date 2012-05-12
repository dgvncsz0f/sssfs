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


module SSSFS.Filesystem.Files
       ( FileHandle()
       , creat
       , open
       , utime
       , fread
       , fwrite
       , fsync
       , ftruncate
       , fstat
       ) where

import           Control.Monad
import qualified Data.ByteString as B
import           SSSFS.Storage
import           SSSFS.Config
import           SSSFS.Filesystem.Types
import           SSSFS.Filesystem.Core
import           SSSFS.Filesystem.Blocks

data FileHandle = FileHandle { unHandle :: INode
                             , shared   :: Bool
                             }

-- | Creates a new empty file
creat :: (StorageHashLike s) => s -> FilePath -> IO FileHandle
creat s path = do { bsz  <- getBlockSize s
                  ; inum <- mknod s bsz path File
                  ; return (FileHandle inum False)
                  }

open :: (StorageHashLike s) => s -> FilePath -> IO FileHandle
open s path = do { inum <- fmap (ensureFile path) (stat s path)
                 ; time <- now
                 ; return (FileHandle (inum { atime = time }) True)
                 }

ftruncate :: (StorageHashLike s) => s -> FileHandle -> Seek -> IO FileHandle
ftruncate s fh0 offset = do { time      <- now
                            ; (fh',fh1) <- fmap (\x -> (x, unHandle x)) (unshare s fh0 transferF)
                            ; return (fh' { unHandle = fh1 { mtime = time
                                                           , ctime = time
                                                           }
                                          })
                            }
  where fh = unHandle fh0

        (blkix, bseek) = address (blksz fh) offset

        transferF = transferOnly blkix bseek

fread :: (StorageHashLike s) => s -> FileHandle -> Seek -> Int -> IO (FileHandle, Block)
fread s fh0 offset sz = do { time <- now
                           ; blk  <- fmap B.concat (sysread s fh offset sz)
                           ; return (fh0 { unHandle = fh { atime = time } }, blk)
                           }
  where fh = unHandle fh0

transferAll :: BlockIx -> Maybe (Block -> Block)
transferAll _ = Just id

transferOnly :: BlockIx -> BlockSeek -> BlockIx -> Maybe (Block -> Block)
transferOnly blkix bseek ix
  | ix < blkix  = Just id
  | ix == blkix = if (bseek > 0) then (Just $ B.take bseek) else Nothing
  | otherwise   = Nothing

copyBlocks :: (StorageHashLike s) => s -> INode -> (BlockIx -> Maybe (Block -> Block)) -> INode -> IO INode
copyBlocks s src mkF dst0 = foldM copyBlock dst0 [0..bsz]
  where bsz = snd (blocks src)

        copyBlock dst ix = case (mkF ix)
                           of Nothing -> return dst
                              Just f  -> fmap f (load s src ix) >>= store s dst ix

unshare :: (StorageHashLike s) => s -> FileHandle -> (BlockIx -> Maybe (Block -> Block)) -> IO FileHandle
unshare s fh0 mkF
    | shared fh0 = fmap fh1 newid >>= fmap (flip FileHandle False) . copyBlocks s (unHandle fh0) mkF
    | otherwise  = return fh0
  where fh1 oid = (unHandle fh0) { blocks = (oid, 0) 
                                 , size   = 0
                                 }


sysread :: (StorageHashLike s) => s -> INode -> Seek -> Int -> IO [Block]
sysread s fh offset sz = do { bHead <- fmap (slice sz bseek) (load s fh blkix)
                            ; bTail <- readNext (B.length bHead)
                            ; return (bHead : bTail)
                            }
  where (blkix, bseek) = address (blksz fh) offset

        readNext 0   = return []
        readNext sz1 = sysread s fh (offset + (fromIntegral sz1)) (sz - sz1)

fstat :: (StorageHashLike s) => s -> FileHandle -> IO INode
fstat _ = return . unHandle

fwrite :: (StorageHashLike s) => s -> FileHandle -> Block -> Seek -> IO FileHandle
fwrite s fh0 raw offset = do { time     <- now
                             ; (fh',fh) <- fmap (\x -> (x, unHandle x)) (unshare s fh0 transferAll)
                             ; nBlocks  <- fmap (chunks fh) (smartLoad fh)
                             ; inum     <- stores s fh (getBlkix fh) nBlocks
                             ; return (fh' { unHandle = inum { mtime = time
                                                             , ctime = time
                                                             }
                                           })
                             }
  where getSize fh = address (blksz fh) offset

        getBlkix = fst . getSize

        bytes = B.length raw

        chunks fh oBlock = toChunks (updateB bseek oBlock raw) (blksz fh)
          where (_, bseek) = getSize fh

        smartLoad fh
            | aligned   = return B.empty
            | otherwise = load s fh blkix
          where (blkix, bseek) = getSize fh
                aligned        = (bseek==0) && (bytes >= blksz fh)

utime :: (StorageHashLike s) => s -> FilePath -> Timestamp -> Timestamp -> IO ()
utime s path uAtime uMtime = do { time <- now
                                ; inum <- stat s path
                                ; sync s (inum { atime = uAtime
                                               , mtime = uMtime
                                               , ctime = time
                                               })
                                }

fsync :: (StorageHashLike s) => s -> FileHandle -> IO FileHandle
fsync s fh0 = do { sync s (unHandle fh0)
                 ; return (fh0 { shared = True } )
                 }