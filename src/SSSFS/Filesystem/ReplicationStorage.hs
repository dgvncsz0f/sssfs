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


module SSSFS.Filesystem.ReplicationStorage
       ( new
       ) where

import Control.Monad
import Control.Exception as C
import SSSFS.Except
import SSSFS.Filesystem.Types
import SSSFS.Storage as S
import SSSFS.Storage.Helpers

data ReplicationStorage a b = ReplicationStorage { master :: a
                                                 , slave  :: b
                                                 }

new :: (StorageHashLike a, StorageHashLike b) => a -> b -> ReplicationStorage a b
new = ReplicationStorage

onSlave :: ReplicationStorage a b -> (b -> c) -> c
onSlave s f = f (slave s)

onMaster :: ReplicationStorage a b -> (a -> c) -> c
onMaster s f = f (master s)

transferBlocks :: (StorageHashLike a, StorageHashLike b, StorageEnumLike a) => ReplicationStorage a b -> INode -> IO ()
transferBlocks s inum = do { inThere <- onMaster s (flip enumKeys (init $ dFromINode inum 0))
                           ; mapM_ (copyNoOverwrite (slave s) (master s)) (dblocks inThere)
                           }
  where nblocks = (snd (blocks inum)) - 1
        dblocks inThere = map (dFromINode inum) (filter ((`notElem` inThere) . ref . show) [0..nblocks])
        
instance (Storage a, Storage b) => Storage (ReplicationStorage a b) where

  shutdown s = onSlave s shutdown >> onMaster s shutdown

instance (StorageHashLike a, StorageEnumLike a, StorageHashLike b) => StorageHashLike (ReplicationStorage a b) where
  
  get s k = C.catch (onSlave s (flip get k)) handler
    where handler e
            | isNotFound e = do { v <- onMaster s (flip get k)
                                ; onSlave s (\s1 -> put s1 k v)
                                ; return v
                                }
            | otherwise    = throw e
  
  head s k = do { r <- onSlave s (flip S.head k)
                ; if (r)
                  then return r
                  else onMaster s (flip S.head k)
                }
  
  del s k = onSlave s (flip del k) >> onMaster s (flip del k)
  
  put s k v
    | keyToINode k  = do { minum <- fmap unitToINode (eulavM v)
                         ; case minum
                           of Nothing
                                -> throw (DataCorruptionExcept "medic!")
                              Just inum
                                -> when (size inum > 0) (transferBlocks s inum)
                         ; onSlave s (\s1 -> put s1 k v)
                         ; onMaster s (\s1 -> put s1 k v)
                         }
    | keyToDBlock k = onSlave s (\s1 -> put s1 k v)
    | otherwise     = onSlave s (\s1 -> put s1 k v) >> onMaster s (\s1 -> put s1 k v)

instance (StorageEnumLike a, StorageEnumLike b) => StorageEnumLike (ReplicationStorage a b) where

  -- | TODO:figure a better way to implement this
  enumKeys s k = onMaster s (flip enumKeys k)
