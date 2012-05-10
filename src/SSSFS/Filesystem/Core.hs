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


module SSSFS.Filesystem.Core
       ( mkfs
       , stat
       , mknod
       , unlink
       , sync
       , load
       , store
       , stores
       ) where

import qualified Data.ByteString as B
import           Data.Maybe
import           Control.Monad (mplus, foldM)
import           Control.Exception
import           SSSFS.Storage
import           SSSFS.Except
import           SSSFS.Filesystem.Types
import           SSSFS.Filesystem.Path

type MakeKey = StorageUnit -> Key

makeKey :: MakeKey
makeKey (INodeUnit o _ _)   = iFromOID o
makeKey (DataBlockUnit o _) = dFromOID o
makeKey (DirEntUnit o _)    = fromStr o

makeKeyWithPrefix :: Key -> MakeKey
makeKeyWithPrefix k = (k++) . makeKey

putUnit :: (StorageHashLike s) => s -> StorageUnit -> MakeKey -> IO Key
putUnit s u f = let k = f u
                in put s k (value u) >> return k

putUnit_ :: (StorageHashLike s) => s -> StorageUnit -> MakeKey -> IO ()
putUnit_ s u f = putUnit s u f >> return ()

getUnit :: (StorageHashLike s) => s -> Key -> IO StorageUnit
getUnit s l = get s l >>= eulavM

follow :: (StorageHashLike s) => s -> Key -> IO INode
follow s l = do { u <- getUnit s l
                ; case u
                  of DirEntUnit _ o
                       -> fmap mUnitToINode (getUnit s (iFromOID o))
                     _
                       -> throw (DataCorruptionExcept "medic!")
                }

-- | Initializes a new filesystem. This is a very fast operation. It
-- creates a new inode with no contents and changes the root
-- inode.
mkfs :: (StorageHashLike s) => s -> IO ()
mkfs s = do { inum <- mkINode (Just oidOne) Directory
            ; putUnit_ s (inodeToUnit inum) makeKey
            ; putUnit_ s (DataBlockUnit oidZero B.empty) makeKey
            }

-- | Creates a new entry on a given directory. The dirname of this
-- path must already be defined and must be a directory already.
-- 
-- This function does not check if there is a link already. It will
-- get overwritten with no mercy. Make sure you do this before calling
-- mknod.
mknod :: (StorageHashLike s) => s -> FilePath -> IType -> IO INode
mknod s path ftype = do { p_inum <- fmap (ensureDirectory dir) (stat s dir)
                        ; inum   <- mkINode Nothing ftype
                        ; putUnit_ s (inodeToUnit inum) makeKey
                        ; putUnit_ s (inodeToDirEntUnit (basename path) inum) (makeKeyWithPrefix (iFromINode p_inum))
                        ; return inum
                        }
  where dir = dirname path

-- | Removes an entry from a given directory. It does not perform any
-- checkings so you better be sure you want to delete this entry. In
-- general this should be the backend of functions suchs as `rmdir'
-- and `unlink'.
unlink :: (StorageHashLike s) => s -> FilePath -> IO ()
unlink s path = do { p_inum <- fmap (ensureDirectory dir) (stat s dir)
                   ; del s (fromDirEnt (inode p_inum) name)
                   }
  where dir = dirname path
        
        name = basename path

sync :: (StorageHashLike s) => s -> INode -> IO ()
sync s inum = do { putUnit_ s (inodeToUnit inum) makeKey
                 }

load :: (StorageHashLike s) => s -> INode -> BlockIx -> IO Block
load s inum ix = fmap unpack (getUnit s (addr blkPtr))
  where blkPtr = fromJust $ getBlock inum ix `mplus` Just (Direct keyZero 0)
        
        unpack (DataBlockUnit _ bytes) = bytes
        unpack _                       = throw (DataCorruptionExcept "SSSFS.Filesystem.Core.load")

store :: (StorageHashLike s) => s -> INode -> BlockIx -> Block -> IO INode
store s inum ix raw = do { oid <- newid
                         ; key <- putUnit s (DataBlockUnit oid raw) makeKey
                         ; return (putBlock inum ix (Direct key (B.length raw)))
                         }

stores :: (StorageHashLike s) => s -> INode -> BlockIx -> [Block] -> IO INode
stores s inum0 ix0 blks = fmap fst (foldM f (inum0, ix0) blks)
  where f (inum,ix) blk = fmap (\a -> (a,ix+1)) (store s inum ix blk)

mUnitToINode :: StorageUnit -> INode
mUnitToINode u = case (unitToINode u)
                 of Just v -> v
                    _      -> throw (DataCorruptionExcept "medic!")




-- | This operation is only defined for absolute paths. The behavior
-- is undefined if you provide a relative path.
stat :: (StorageHashLike s) => s -> FilePath -> IO INode
stat s p = do { root <- fmap mUnitToINode (getUnit s keyOne)
              ; stat_ root (tail $ safeSplitPath p)
              }
  where stat_ inum []        = return inum
        stat_ inum (x:xs)
          | isDirectory inum = follow s (fromDirEnt (inode inum) x) >>= flip stat_ xs
          | otherwise        = throw (NotADir p)

