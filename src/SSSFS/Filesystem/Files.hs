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

import qualified Data.ByteString as B
import           SSSFS.Storage
import           SSSFS.Filesystem.Types
import           SSSFS.Filesystem.Core
import           SSSFS.Filesystem.Blocks

newtype FileHandle = FileHandle { unHandle :: INode }

-- | Creates a new empty file
creat :: (StorageHashLike s) => s -> FilePath -> IO FileHandle
creat s path = do { inum <- mknod s path File
                  ; return (FileHandle inum)
                  }

open :: (StorageHashLike s) => s -> FilePath -> IO FileHandle
open s path = do { inum <- fmap (ensureFile path) (stat s path)
                 ; time <- now
                 ; return (FileHandle $ inum { atime = time })
                 }

ftruncate :: (StorageHashLike s) => s -> FileHandle -> Seek -> IO FileHandle
ftruncate s fh0 offset = do { time  <- now
                            ; bLast <- fmap (truncateB bseek) (load s fh blkix)
                            ; ufh   <- store s (truncateI fh blkix) blkix bLast
                            ; return (FileHandle $ ufh { mtime = time
                                                       , ctime = time
                                                       })
                            }
  where fh = unHandle fh0
        
        (blkix, bseek) = address (blksz fh) offset

fread :: (StorageHashLike s) => s -> FileHandle -> Seek -> Int -> IO (FileHandle, Block)
fread s fh0 offset sz = do { time <- now
                           ; blk  <- fmap B.concat (sysread s fh offset sz)
                           ; return (FileHandle $ fh { atime = time }, blk)
                           }
  where fh = unHandle fh0


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
fwrite s fh0 raw offset = do { time    <- now
                             ; nBlocks <- fmap chunks smartLoad
                             ; inum    <- stores s fh blkix nBlocks
                             ; return (FileHandle $ inum { mtime = time
                                                         , ctime = time
                                                         })
                             }
  where fh = unHandle fh0
    
        (blkix, bseek) = address (blksz fh) offset
        
        chunks oBlock = toChunks (updateB bseek oBlock raw) (blksz fh)
        
        bytes = B.length raw
        
        aligned = (bseek==0) && (bytes >= blksz fh)
        
        smartLoad
          | aligned   = return B.empty
          | otherwise = load s fh blkix
        
utime :: (StorageHashLike s) => s -> FilePath -> Timestamp -> Timestamp -> IO ()
utime s path uAtime uMtime = do { time <- now
                                ; inum <- stat s path
                                ; sync s (inum { atime = uAtime
                                               , mtime = uMtime
                                               , ctime = time
                                               })
                                }

fsync :: (StorageHashLike s) => s -> FileHandle -> IO ()
fsync s = sync s . unHandle