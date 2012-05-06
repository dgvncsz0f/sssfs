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
       ( FileHandle
       , creat
       , open
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

type FileHandle = INode

-- | Creates a new empty file
creat :: (StorageHashLike s) => s -> FilePath -> IO FileHandle
creat s path = do { inum <- mknod s path File
                  ; return inum
                  }

open :: (StorageHashLike s) => s -> FilePath -> IO FileHandle
open s path = do { inum <- fmap (ensureFile path) (stat s path)
                 ; return inum
                 }

ftruncate :: (StorageHashLike s) => s -> FileHandle -> Seek -> IO FileHandle
ftruncate s fh offset = do { bLast <- fmap (truncateB seek) (load s fh blkIx)
                           ; store s (truncateI fh blkIx) blkIx bLast
                           }
  where (blkIx, seek) = calc (blksz fh) offset

fread :: (StorageHashLike s) => s -> FileHandle -> Seek -> Size -> IO Block
fread s fh offset bufsz = fmap B.concat (freadChunks s fh offset bufsz)

freadChunks :: (StorageHashLike s) => s -> FileHandle -> Seek -> Size -> IO [Block]
freadChunks s fh offset bufsz = do { bHead <- fmap (slice bufsz seek) (load s fh blkIx)
                                   ; bTail <- readTail (B.length bHead)
                                   ; return (bHead : bTail)
                                   }
  where (blkIx, seek) = calc (blksz fh) offset

        readTail 0      = return []
        readTail bCount = freadChunks s fh (offset + (fromIntegral bCount)) (bufsz - bCount)

fstat :: (StorageHashLike s) => s -> FileHandle -> IO INode
fstat _ = return

fwrite :: (StorageHashLike s) => s -> FileHandle -> Block -> Seek -> IO FileHandle
fwrite s fh raw offset = do { nBlocks <- fmap chunks (load s fh blkIx)
                            ; inum    <- stores s fh blkIx nBlocks
                            ; return inum
                            }
  where (blkIx, seek) = calc (blksz fh) offset
        
        chunks oBlock = toChunks (writeB seek oBlock raw) (blksz fh)
        

fsync :: (StorageHashLike s) => s -> FileHandle -> IO ()
fsync s fh = sync s fh