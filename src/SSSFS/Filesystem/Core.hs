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
       , unlinkNod
       ) where

import Control.Exception
import SSSFS.Storage
import SSSFS.Except
import SSSFS.Filesystem.Types
import SSSFS.Filesystem.Path

type MakeKey = Either Key (OID -> Key) -> Key

makeKey :: (Maybe OID) -> MakeKey
makeKey _ (Left k)         = k
makeKey Nothing (Right _)  = error "cant create key without oid"
makeKey (Just o) (Right f) = f o

makeKey_ :: MakeKey
makeKey_ = makeKey Nothing

makeKey1 :: OID -> MakeKey
makeKey1 = makeKey . Just

putUnit :: (StorageHashLike s) => s -> StorageUnit -> MakeKey -> IO Key
putUnit s u f = let k = f $ fromStorageUnit u
                in put s k (value u) >> return k

putUnit_ :: (StorageHashLike s) => s -> StorageUnit -> MakeKey -> IO ()
putUnit_ s u f = let k = f $ fromStorageUnit u
                 in put s k (value u)

getUnit :: (StorageHashLike s) => s -> Key -> IO StorageUnit
getUnit s l = get s l >>= eulavM

follow :: (StorageHashLike s) => s -> Key -> IO INode
follow s l = getUnit s l >>= followUM
  where followU (INodePtrUnit _ l2) = fmap inodeUnitToINode (getUnit s l2)
        followU (DirEntUnit _ o)    = getUnit s (fromOID o) >>= followU
        followU _                   = return Nothing
        
        followUM u = do { mv <- followU u
                        ; case mv 
                          of Nothing
                               -> throw (DataCorruptionExcept "SSSFS.Filesystem.Core.follow")
                             Just v
                               -> return v
                        }

-- | Initializes a new filesystem. This is a very fast operation. It
-- creates a new inode with no contents and changes the root inode
-- ptr, which will point to the newly created inode. Therefore old
-- data will still be preserved but from now on is unreachable.
mkfs :: (StorageHashLike s) => s -> IO ()
mkfs s = do { inum <- mkINode (Just oidOne) Directory
            ; l    <- putUnit s (inodeToINodeUnit inum) makeKey_
            ; putUnit_ s (inodeToINodePtrUnit inum l) makeKey_
            ; putUnit_ s emptyBlock makeKey_
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
                        ; l      <- putUnit s (inodeToINodeUnit inum) makeKey_
                        ; putUnit_ s (inodeToINodePtrUnit inum l) makeKey_
                        ; putUnit_ s (inodeToDirEntUnit (basename path) inum) (makeKey1 (inode p_inum))
                        ; return inum
                        }
  where dir = dirname path

-- | Removes an entry from a given directory. It does not perform any
-- checkings so you better be sure you want to delete this entry. In
-- general this should be the backend of functions suchs as `rmdir'
-- and `unlink'.
unlinkNod :: (StorageHashLike s, StorageEnumLike s) => s -> FilePath -> IO ()
unlinkNod s path = do { p_inum <- fmap (ensureDirectory dir) (stat s dir)
                      ; del s (fromLinkName (inode p_inum) (basename path))
                      }
  where dir = dirname path

-- | This operation is only defined for absolute paths. The behavior
-- is undefined if you provide a relative path.
stat :: (StorageHashLike s) => s -> FilePath -> IO INode
stat s p = do { root <- follow s keyOne
              ; stat_ root (tail $ safeSplitPath p)
              }
  where stat_ inum []        = return inum
        stat_ inum (x:xs)
          | isDirectory inum = follow s (fromLinkName (inode inum) x) >>= flip stat_ xs
          | otherwise        = throw (NotADir p)

